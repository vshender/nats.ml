(** NATS client. *)

open Unix

open Nats.Parse
open Nats.Serialize
open Nats.Protocol

type t = {
  sock           : file_descr;
  mutable ssid   : int;
  bytes          : Bytes.t;
  reader         : Reader.t;
  serializer     : Faraday.t;
}

let read_client_msg ?timeout c =
  let start_time = gettimeofday () in
  let time_remaining () =
    Option.map
      (fun timeout -> Float.max (timeout -. (gettimeofday () -. start_time)) 0.)
      timeout
  in

  let rec loop () =
    match Reader.next_msg c.reader with
    | Reader.Message msg ->
      msg
    | Reader.Need_input ->
      Socket.read
        ?timeout:(time_remaining ())
        c.sock
        c.bytes 0 (Bytes.length c.bytes)
      |> begin function
        | 0 -> failwith "closed"
        | n ->
          Reader.feed c.reader c.bytes 0 n;
          loop ()
      end
    | Reader.Parse_error (_, msg) ->
      failwith msg
  in
  loop ()

let read_msg ?timeout c =
  try
    Some (read_client_msg ?timeout c)
  with
  | Socket.Timed_out -> None


let rec flush_output c =
  match Faraday.operation c.serializer with
  | `Writev (iovec :: _) ->
    begin
      match write_bigarray c.sock iovec.buffer iovec.off iovec.len with
      | 0 ->
        failwith "send_msg: connection failure"
      | n ->
        Faraday.shift c.serializer n;
        flush_output c
    end
  | `Writev [] -> assert false
  | `Yield     -> ()
  | `Close     -> ()

let send_msg c msg =
  serialize_client_message c.serializer msg;
  flush_output c

let connect
    ?(url = "nats://127.0.0.1:4222")
    ?(name = "nats.ml-unix")
    ?(verbose = false)
    ?(pedantic = false)
    ?connect_timeout
    ?(keepalive = false)  (* ??? *)
    () =
  let open Unix in

  let uri = Uri.of_string url in
  let hostname = Uri.host uri |> Option.value ~default:"127.0.0.1"
  and port = Uri.port uri |> Option.value ~default:4222 in

  let start_time = gettimeofday () in
  let time_remaining () =
    Option.map
      (fun timeout -> Float.max (timeout -. (gettimeofday () -. start_time)) 0.)
      connect_timeout
  in

  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock TCP_NODELAY true in
  if keepalive then
    ignore (setsockopt sock SO_KEEPALIVE true);

  let addr = ADDR_INET (inet_addr_of_string hostname, port) in
  Socket.connect ?timeout:(time_remaining ()) sock addr;

  let conn = {
    sock;
    ssid       = 0;
    bytes      = Bytes.create 0x1000;
    reader     = Reader.create ();
    serializer = Faraday.create 0x1000;
  } in

  let info = read_client_msg ?timeout:(time_remaining ()) conn in
  begin match info with
    | ServerMessage.Info info ->
      if info.Info.tls_required then
        failwith "client doensn't support TLS";
    | _ -> assert false;
  end;

  let connect = ClientMessage.Connect
      (Connect.make
         ~name:name
         ~lang:"ocaml"
         ~protocol:0
         ~version:"0.0.1"
         ~verbose:verbose
         ~pedantic:pedantic
         ~tls_required:false
         ~echo:true
         ~no_responders:false
         ~headers:false
         ())
  in
  send_msg conn connect;

  conn

let subscribe c ?group subject =
  c.ssid <- c.ssid + 1;
  let sub = ClientMessage.Sub
      (Sub.make ~subject ?group ~sid:(string_of_int c.ssid) ())
  in
  send_msg c sub

let publish c subject payload =
  let pub = ClientMessage.Pub (Pub.make ~subject ~payload ()) in
  send_msg c pub

let close c =
  shutdown c.sock SHUTDOWN_ALL;
  close c.sock;
  Faraday.close c.serializer;
  Faraday.drain c.serializer
