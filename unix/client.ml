(** NATS client. *)

open Compat

open Unix

open Nats.Parse
open Nats.Serialize
open Nats.Protocol

let client_name    = "nats.ml-unix"
let client_lang    = "ocaml"
let client_version = "0.0.1-dev"

type callback = Msg.t -> unit
type error_callback = exn -> unit

(** A module to track the number of unacknowledged PINGs. *)
module PingPongTracker = struct
  (** The type representing the state of unacknowledged PINGs. *)
  type t = {
    mutable counter : int;
    mutex           : Mutex.t;
    condition       : Condition.t;
  }

  (** [create ()] initializes a new tracker for unacknowledged PINGs. *)
  let create () = {
    counter   = 0;
    mutex     = Mutex.create ();
    condition = Condition.create ();
  }

  (** [incr_and_wait t] increments the PING counter and waits until all PONGs
      are received. *)
  let incr_and_wait t =
    Mutex.protect t.mutex
      begin fun () ->
        t.counter <- t.counter + 1;
        while t.counter > 0 do
          Condition.wait t.condition t.mutex
        done
      end

  (** [decr t] decrements the PING counter and signals when all PONGs are
      received. *)
  let decr t =
    Mutex.protect t.mutex
      begin fun () ->
        t.counter <- t.counter - 1;
        if t.counter = 0 then
          Condition.signal t.condition
      end
end

type t = {
  mutex             : Mutex.t;
  sock              : file_descr;
  in_buffer         : Bytes.t;
  reader            : Reader.t;
  serializer        : Faraday.t;
  mutable ssid      : int;
  subscriptions     : (int, callback) Hashtbl.t;
  mutable running   : bool;
  mutable io_thread : Thread.t;
  ping_pongs        : PingPongTracker.t;
  error_cb          : error_callback;
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
        c.in_buffer 0 (Bytes.length c.in_buffer)
      |> begin function
        | 0 ->
          c.running <- false;
          failwith "closed"
        | n ->
          Reader.feed c.reader c.in_buffer 0 n;
          loop ()
      end
    | Reader.Parse_error (_, msg) ->
      failwith msg
  in
  loop ()

let send_msg c msg =
  Mutex.protect c.mutex
    (fun () -> serialize_client_message c.serializer msg)

let rec io_loop c =
  let select_timeout = 0.1 in

  try
    while c.running do
      try
        let (readable, writeable, _) = select [c.sock] [c.sock] [] select_timeout in

        (* Read incoming data *)
        if List.length readable > 0 then begin
          match read c.sock c.in_buffer 0 (Bytes.length c.in_buffer) with
          | 0 ->
            c.error_cb (Failure "connection closed");
            c.running <- false;
          | n ->
            let msg =  (* TODO: result instead of option *)
              Mutex.protect c.mutex
                begin fun () ->
                  Reader.feed c.reader c.in_buffer 0 n;
                  match Reader.next_msg c.reader with
                  | Reader.Message msg -> Some msg
                  | Reader.Need_input  -> None
                  | Reader.Parse_error (_, msg) ->
                    c.error_cb (Failure msg);
                    c.running <- false;
                    None
                end
            in Option.iter (dispatch_message c) msg
        end;

        (* Send outgoing data *)
        if List.length writeable > 0 then begin
          let iovec =
            Mutex.protect c.mutex
              begin fun () ->
                match Faraday.operation c.serializer with
                | `Writev (iovec :: _) -> Some iovec
                | _                   -> None
              end
          in
          Option.iter
            begin fun iovec ->
              match write_bigarray c.sock iovec.Faraday.buffer iovec.off iovec.len with
              | 0 ->
                c.error_cb (Failure "connection failure");
                c.running <- false;
              | n ->
                Mutex.protect c.mutex (fun () -> Faraday.shift c.serializer n)
            end
            iovec
        end;

        (* TODO: process timeout *)

        (* TODO: send pings *)
      with
      | _ -> Thread.yield ()  (* TODO: ??? *)
    done
  with exn ->
    Printf.eprintf "I/O thread execution: %s\n%!" (Printexc.to_string exn)

and dispatch_message c = function
  | ServerMessage.Msg msg ->
    begin
      (* Regular subscription *)
      let sid = int_of_string msg.sid in
      let callback = Mutex.protect c.mutex
          (fun () -> Hashtbl.find_opt c.subscriptions sid)
      in
      match callback with
      | Some cb -> cb msg
      | None    -> ()
    end
  | ServerMessage.Ping ->
    send_msg c ClientMessage.Pong
  | ServerMessage.Pong ->
    PingPongTracker.decr c.ping_pongs
  | ServerMessage.Info _ ->
    ()  (* TODO: handle updated info *)
  | ServerMessage.Err msg ->
    c.error_cb (Failure msg)
  | ServerMessage.Ok -> ()
  | _ -> ()

let default_error_callback exn =
  Printf.fprintf
    Stdlib.stderr
    "NATS: encoutered error %s\n%!" (Printexc.to_string exn)

let connect
    ?(url = "nats://127.0.0.1:4222")
    ?(name = client_name)
    ?(verbose = false)
    ?(pedantic = false)
    ?connect_timeout
    ?(keepalive = false)  (* ??? *)
    ?(error_cb = default_error_callback)
    () =
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
    mutex            = Mutex.create ();
    sock;
    in_buffer        = Bytes.create 0x1000;
    reader           = Reader.create ();
    serializer       = Faraday.create 0x1000;
    ssid             = 0;
    subscriptions    = Hashtbl.create 32;
    running          = true;
    io_thread        = Thread.self ();
    ping_pongs       = PingPongTracker.create ();

    error_cb         = error_cb;
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
         ~lang:client_lang
         ~protocol:0
         ~version:client_version
         ~verbose:verbose
         ~pedantic:pedantic
         ~tls_required:false
         ~echo:true
         ~no_responders:false
         ~headers:false
         ())
  in
  send_msg conn connect;

  conn.io_thread <- Thread.create io_loop conn;

  conn

let subscribe c ?group subject callback =
  let sub =
    Mutex.protect c.mutex
      begin fun () ->
        c.ssid <- c.ssid + 1;
        Hashtbl.add c.subscriptions c.ssid callback;
        ClientMessage.Sub
          (Sub.make ~subject ?group ~sid:(string_of_int c.ssid) ())
      end
  in send_msg c sub

let publish c subject payload =
  let pub = ClientMessage.Pub (Pub.make ~subject ~payload ()) in
  send_msg c pub

let flush c =
  send_msg c ClientMessage.Ping;
  PingPongTracker.incr_and_wait c.ping_pongs

let close c =
  c.running <- false;
  Thread.join c.io_thread;
  shutdown c.sock SHUTDOWN_ALL;
  close c.sock;
  Faraday.close c.serializer;
  ignore (Faraday.drain c.serializer);
  ()
