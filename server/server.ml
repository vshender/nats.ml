(** The module for managing a local instance of the NATS server for testing
    purposes. *)

open Unix

open Nats.Parse
open Nats.Protocol

let nats_server_command = "nats-server"
let default_hostname    = "127.0.0.1"

(** A regular expression to match client connection log entries. *)
let client_re = Str.regexp "client connections on [0-9.]*:\\([0-9]*\\)"

(** The type representing a NATS server instance. *)
type t = {
  id : string;
  (** The unique identifier for the server instance. *)
  port : int option;
  (** The port on which the server listens for client connections.  If [None],
      the port will be chosen automatically. *)
  mutable pid : int option;
  (** The process ID of the running server instance, if any. *)
}

let create ?id ?port () =
  let id = match id with
    | Some id -> id
    | None    -> Nuid.next ()
  in
  { id; port; pid = None }

let pid { pid; _ } = pid

(** [logfile server] returns the path to the server's log file. *)
let logfile { id; _ } =
  let tmpdir = Filename.get_temp_dir_name () in
  Filename.concat tmpdir @@ Printf.sprintf "nats-server-%s.log" id

let start server =
  match server.pid with
  | Some _ -> failwith "NATS server is already running"
  | None   ->
    let pid = create_process
        nats_server_command
        [|
          nats_server_command;
          "-p"; string_of_int @@ Option.value server.port ~default:(-1);
          "-l"; logfile server;
        |]
        stdin stdout stderr
    in
    server.pid <- Some pid

let stop server =
  match server.pid with
  | None     -> failwith "NATS server is not running"
  | Some pid ->
    try
      server.pid <- None;
      kill pid Sys.sigterm;
      ignore @@ waitpid [] pid;
      try unlink (logfile server) with Unix_error(ENOENT, _, _) -> ()
    with Unix_error (ESRCH, _, _) ->
      ()

let with_server ?id ?port f =
  let server = create ?id ?port () in
  start server;
  Fun.protect
    ~finally:(fun () -> stop server)
    (fun () -> f server)

let is_running server =
  match server.pid with
  | None     -> false
  | Some pid ->
    try
      ignore @@ kill pid 0;
      true
    with Unix_error (ESRCH, _, _) ->
      false

(** [client_port server] retrieves the client port number from the server log
    file.

    Raises [Failure] if the port information cannot be found. *)
let client_port server =
  (* We may need to wait for log to be present.  Wait up to 5s. *)
  let rec loop = function
    | 0 -> failwith "no client port info"
    | n ->
      try
        In_channel.with_open_text (logfile server) @@ fun ic ->
        let log = In_channel.input_all ic in
        ignore @@ Str.search_forward client_re log 0;
        int_of_string @@ Str.matched_group 1 log
      with Sys_error _ | Not_found ->
        Thread.delay 0.1;
        loop (n - 1)
  in loop 50

let client_url server =
  if not (is_running server) then
    failwith "NATS server is not running";

  let port = client_port server in
  let addr = ADDR_INET (inet_addr_of_string default_hostname, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  begin
    try
      Unix.connect sock addr;
    with
      Unix_error (ECONNREFUSED, _, _) -> failwith "NATS server connection refused"
  end;

  Fun.protect
    begin fun () ->
      let buf = Bytes.create 0x1000
      and reader = Reader.create () in
      let n =
        try
          read sock buf 0 (Bytes.length buf)
        with
          Unix_error (ECONNRESET, _, _) -> failwith "NATS server connection lost"
      in

      Reader.feed reader buf 0 n;
      match Reader.next_msg reader with
      | Reader.Message (ServerMessage.Info info) ->
        let scheme = if info.tls_required then "tls" else "nats" in
        Printf.sprintf "%s://%s:%d" scheme default_hostname port
      | _ -> failwith "client_url: did not receive INFO"
    end
    ~finally:begin fun () ->
      begin
        try
          shutdown sock SHUTDOWN_ALL
        with Unix_error (ENOTCONN, _, _) ->
          ()
      end;
      close sock
    end
