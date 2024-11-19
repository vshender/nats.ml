open Unix

open Lwt.Infix
open Lwt.Syntax

open Nats.Errors
open Nats.Parse
open Nats.Serialize
open Nats.Protocol

let client_name    = "nats.ml-lwt"
let client_lang    = "ocaml"
let client_version = "0.0.1-dev"

let default_hostname = "127.0.0.1"
let default_port     = 4222

let default_url = Printf.sprintf "nats://%s:%d" default_hostname default_port

let with_timeout ?timeout f =
  match timeout with
  | None         -> f ()
  | Some timeout ->
    Lwt.catch
      (fun () -> Lwt_unix.with_timeout timeout f)
      (function
        | Lwt_unix.Timeout -> nats_error Timeout
        | exc              -> raise exc)

(** The type representing the state of the NATS connection. *)
type state =
  | Connected
  (** [Connected] indicates that the client is successfully connected to the
      server. *)
  | Closing
  (** [Closing] indicates that the client is in the process of closing the
      connection.  This state tells the I/O and message processing threads to
      stop working.  The client enters the [Closed] state only after completing
      these threads. *)
  | Closed
  (** [Closed] indicates that the client connection is closed. *)

(** The type of NATS client. *)
type t = {
  options : options;
  (** The client configuration options. *)
  mutable state : state;
  (** The client state. *)
  sock : Lwt_unix.file_descr;
  (** The socket file descriptor for the connection. *)
  in_buffer : Bytes.t;
  (** A buffer for reading incoming data. *)
  reader : Reader.t;
  (** The reader for parsing incoming messages. *)
}

(** The NATS client configuration options. *)
and options = {
  url : string;
  (** A NATS server URL to which the client will be connecting. *)
  name : string;
  (** An optional name label which will be sent to the server on CONNECT to
      identify the client. *)
  verbose : bool;
  (** Signals the server to send an OK ack for commands successfully processed
      by the server. *)
  pedantic : bool;
  (** Signals the server whether it should be doing further validation of
      subjects. *)
  connect_timeout : float option;
  (** The timeout for a connection operation to complete. *)
}

(** [send_msg_direct c msg] serializes the given message [msg] and sends it
    directly to the server by writing it to the client's socket.  This function
    blocks the current thread until the message is successfully sent. *)
let send_msg_direct c msg =
  let serializer = Faraday.create 0x100 in
  serialize_client_message serializer msg;
  let data = Faraday.serialize_to_string serializer in
  Lwt.catch
    (fun () ->
       Lwt_unix.write_string c.sock data 0 (String.length data) >>= fun _ ->
       Lwt.return ())
    (function
      | Unix_error (EPIPE, _, _) -> nats_error ConnectionLost
      | exc                      -> raise exc)

(** [remaining_time_fn timeout] returns a function that calculates the time
    remaining from the given [timeout] in seconds.

    The returned function returns [None] if no timeout was specified, or the
    remaining time otherwise. *)
let remaining_time_fn timeout =
  let start_time = gettimeofday () in
  fun () ->
    Option.map
      (fun timeout -> Float.max (timeout -. (gettimeofday () -. start_time)) 0.)
      timeout

(** [read_client_msg ?timeout c] reads the next message from the server for the
    client [c].  If a [timeout] is provided, the operation will fail if the
    timeout is exceeded.

    Raises:

    - [NatsError ConnectionClosed] if the connection is closed.
    - [NatsError Timeout] if the reading times out.
    - [NatsError (ProtocolError e)] if there is an error parsing the message.
*)
let read_client_msg ?timeout c =
  let remaining_time = remaining_time_fn timeout in

  let rec loop () =
    match Reader.next_msg c.reader with
    | Reader.Message msg ->
      Printf.printf "MSG: %s\n%!" @@ ServerMessage.show msg;
      Lwt.return msg
    | Reader.Need_input ->
      with_timeout
        (fun () -> Lwt_unix.read c.sock c.in_buffer 0 (Bytes.length c.in_buffer))
        ?timeout:(remaining_time ()) >>= begin function
        | 0 -> nats_error ConnectionLost
        | n ->
          Reader.feed c.reader c.in_buffer 0 n;
          loop ()
      end
    | Reader.Parse_error (_, e) ->
      nats_error (ProtocolError e)
  in
  Lwt.catch
    loop
    (function
      | Unix_error (ECONNRESET, _, _) -> nats_error ConnectionLost
      | exc                           -> raise exc)

(** [process_expected_info ?timeout c] looks for the expected first INFO
    message sent when a connection is established. *)
let process_expected_info ?timeout c =
  read_client_msg c ?timeout >>= function
  | ServerMessage.Info info ->
    if info.Info.tls_required then
      failwith "client doesn't support TLS";
    Lwt.return ()
  | _ ->
    nats_error NoInfoReceived

(** [send_connect ?timeout c] sends a CONNECT protocol message to the server
    and waits for a flush to return from the server for error processing. *)
let send_connect ?timeout c =
  let remaining_time = remaining_time_fn timeout in

  let connect_msg = ClientMessage.Connect
      (Connect.make
         ~name:c.options.name
         ~lang:client_lang
         ~version:client_version
         ~protocol:0
         ~verbose:c.options.verbose
         ~pedantic:c.options.pedantic
         ~tls_required:false
         ~echo:true
         ~no_responders:false
         ~headers:false
         ())
  in
  let* () = send_msg_direct c connect_msg in

  let* () = send_msg_direct c ClientMessage.Ping in
  let* msg = read_client_msg c ?timeout:(remaining_time ()) in
  let* msg =
    if c.options.verbose && msg = ServerMessage.Ok then
      read_client_msg c ?timeout:(remaining_time ())
    else
      Lwt.return msg
  in
  match msg with
  | ServerMessage.Pong    -> Lwt.return ()
  | ServerMessage.Err msg -> nats_error (parse_server_err_msg msg)
  | _                     -> nats_error (UnexpectedProtocol msg)

let connect
    ?(url = default_url)
    ?(name = client_name)
    ?(verbose = false)
    ?(pedantic = false)
    ?connect_timeout
    () =
  let options = {
    url;
    name;
    verbose;
    pedantic;
    connect_timeout;
  } in

  let uri = Uri.of_string options.url in
  let hostname = Uri.host uri |> Option.value ~default:default_hostname
  and port     = Uri.port uri |> Option.value ~default:default_port in

  let remaining_time = remaining_time_fn options.connect_timeout in

  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let _ = Lwt_unix.setsockopt sock TCP_NODELAY true in

  let addr = ADDR_INET (inet_addr_of_string hostname, port) in
  let* () = Lwt.catch
      begin fun () ->
        with_timeout
          (fun () -> Lwt_unix.connect sock addr)
          ?timeout:(remaining_time ())
      end
      (function
        | Unix_error (ECONNREFUSED, _, _) -> nats_error NoServers
        | exc                             -> raise exc)
  in

  let conn = {
    options;
    state     = Connected;
    sock;
    in_buffer = Bytes.create 0x1000;
    reader    = Reader.create ();
  } in

  let* () = Lwt.catch
      begin fun () ->
        let* () = process_expected_info conn ?timeout:(remaining_time ()) in
        let* () = send_connect conn ?timeout:(remaining_time ()) in
        Lwt.return ()
      end
      begin fun exc ->
        begin
          try
            Lwt_unix.shutdown conn.sock SHUTDOWN_ALL
          with Unix_error (ENOTCONN, _, _) ->
            ()
        end;
        let* () = Lwt_unix.close conn.sock in
        (* Faraday.close conn.serializer; *)
        raise exc
      end
  in

  Lwt.return conn
