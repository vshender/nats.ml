(** NATS client. *)

open Compat

open Unix

open Nats.Parse
open Nats.Serialize
open Nats.Protocol

let client_name    = "nats.ml-unix"
let client_lang    = "ocaml"
let client_version = "0.0.1-dev"

type callback = Subscription.callback
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

module CurrentSyncOperation = struct
  type cur_sync_op = {
    id             : int;
    timeout_time   : float option;
    signal_timeout : unit -> unit;
  }

  type t = {
    mutable cur_sync_op : cur_sync_op option;
    mutex               : Mutex.t;
  }

  let create () = {
    cur_sync_op = None;
    mutex       = Mutex.create ();
  }

  let start t ~id ~timeout_time ~signal_timeout =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | None   -> t.cur_sync_op <- Some { id; timeout_time; signal_timeout }
        | Some _ -> failwith "another sync operation is running"
      end

  let finish t ~id =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | Some { id = oid; _ } when id = oid ->
          t.cur_sync_op <- None
        | Some _ -> failwith "another sync operation is running"
        | None   -> failwith "no sync operation is running"
      end

  let check_timeout t =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | Some { timeout_time = Some ttime; signal_timeout; _ } ->
          if gettimeofday () >= ttime then signal_timeout ()
        | Some { timeout_time = None; _ } | None ->
          ()
      end
end

module Subscriptions = struct
  type t = {
    mutable ssid : int;
    subs         : (int, Subscription.t) Hashtbl.t;
    mutex        : Mutex.t;
  }

  let create () = {
    ssid  = 0;
    subs  = Hashtbl.create 32;
    mutex = Mutex.create ();
  }

  let iter t f =
    Mutex.protect t.mutex
      (fun () -> Hashtbl.iter (fun _sid sub -> f sub) t.subs)

  let subscribe
      t subject group callback
      unsubscribe_cb remove_subscription_cb
      next_msg_start_cb next_msg_finish_cb
    =
    Mutex.protect t.mutex
      begin fun () ->
        t.ssid <- t.ssid + 1;
        let sub = Subscription.create
            ~unsubscribe_cb ~remove_subscription_cb
            ~next_msg_start_cb ~next_msg_finish_cb
            t.ssid subject group callback
        in
        Hashtbl.add t.subs (Subscription.sid sub) sub;
        sub
      end

  let unsubscribe t sub =
    Mutex.protect t.mutex
      (fun () -> Hashtbl.remove t.subs (Subscription.sid sub))

  let unsubscribe_all t =
    Mutex.protect t.mutex
      (fun () -> Hashtbl.clear t.subs)

  let handle_msg t msg =
    let sub = Mutex.protect t.mutex
        (fun () -> Hashtbl.find_opt t.subs msg.Message.sid)
    in
    match sub with
    | Some sub -> Subscription.handle_msg sub msg
    | None     -> ()
end

type t = {
  mutex             : Mutex.t;
  sock              : file_descr;
  in_buffer         : Bytes.t;
  reader            : Reader.t;
  serializer        : Faraday.t;
  subscriptions     : Subscriptions.t;
  mutable running   : bool;
  mutable io_thread : Thread.t;
  ping_pongs        : PingPongTracker.t;
  cur_sync_op       : CurrentSyncOperation.t;
  error_cb          : error_callback;
}

let is_running c =
  c.running

let is_closed c =
  not c.running

let send_msg c msg =
  if is_closed c then
    failwith "connection closed";

  Mutex.protect c.mutex
    (fun () -> serialize_client_message c.serializer msg)

let flush c =
  if is_closed c then
    failwith "connection closed";

  send_msg c ClientMessage.Ping;
  PingPongTracker.incr_and_wait c.ping_pongs

let close c =
  Mutex.protect c.mutex
    (fun () -> c.running <- false);
  Thread.join c.io_thread

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
          close c;
          failwith "connection closed"
        | n ->
          Reader.feed c.reader c.in_buffer 0 n;
          loop ()
      end
    | Reader.Parse_error (_, msg) ->
      failwith msg
  in
  loop ()

let rec io_loop c =
  let exception Close in
  let select_timeout = 0.05 in

  try
    Fun.protect
      ~finally:begin fun () ->
        Mutex.protect c.mutex
          begin fun () ->
            Subscriptions.iter c.subscriptions Subscription.close;
            Subscriptions.unsubscribe_all c.subscriptions;
            shutdown c.sock SHUTDOWN_ALL;
            Unix.close c.sock;
            Faraday.close c.serializer;
            ignore (Faraday.drain c.serializer)
          end
      end
      begin fun () ->
        while is_running c do
          (* Read incoming data *)
          let (readable, _, _) = select [c.sock] [] [] select_timeout in
          if List.length readable > 0 then begin
            match read c.sock c.in_buffer 0 (Bytes.length c.in_buffer) with
            | 0 ->
              c.error_cb (Failure "connection closed");
              raise Close
            | n ->
              Mutex.protect c.mutex
                (fun () -> Reader.feed c.reader c.in_buffer 0 n)
          end;

          (* Send outgoing data *)
          let (_, writeable, _) = select [] [c.sock] [] select_timeout in
          if List.length writeable > 0 then begin
            Mutex.protect c.mutex
              begin fun () ->
                match Faraday.operation c.serializer with
                | `Writev (iovec :: _) -> Some iovec
                | _                   -> None
              end
            |> Option.iter
              begin fun iovec ->
                match
                  write_bigarray
                    c.sock
                    iovec.Faraday.buffer iovec.off iovec.len
                with
                | 0 ->
                  c.error_cb (Failure "connection failure");
                  raise Close
                | n ->
                  Mutex.protect c.mutex
                    (fun () -> Faraday.shift c.serializer n)
              end
          end;

          (* Process the current operation's timeout. *)
          CurrentSyncOperation.check_timeout c.cur_sync_op;

          (* Process messages. *)
          let rec msg_processing_loop () =
            let next_msg = Mutex.protect c.mutex
                (fun () -> Reader.next_msg c.reader)
            in
            match next_msg with
            | Reader.Message msg ->
              dispatch_message c msg;
              msg_processing_loop ()
            | Reader.Need_input ->
              ()
            | Reader.Parse_error (_, msg) ->
              c.error_cb (Failure msg);
              raise Close
          in msg_processing_loop ();

          (* TODO: send pings *)
        done
      end
  with
  | Close -> ()
  | exn -> c.error_cb exn

and dispatch_message c = function
  | ServerMessage.Msg msg ->
    Subscriptions.handle_msg c.subscriptions {
      subject = msg.Msg.subject;
      reply   = msg.Msg.reply;
      sid     = int_of_string msg.Msg.sid;
      headers = None;
      payload = msg.Msg.payload;
    }
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
    subscriptions    = Subscriptions.create ();
    running          = true;
    io_thread        = Thread.self ();
    ping_pongs       = PingPongTracker.create ();
    cur_sync_op      = CurrentSyncOperation.create ();
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

let unsubscribe_cb c sub =
  if is_closed c then
    failwith "connection closed";
  let unsub_msg = ClientMessage.UnSub
      (UnSub.make
         ~sid:(string_of_int (Subscription.sid sub))
         ?max_msgs:(Subscription.max_msgs sub)
         ())
  in
  send_msg c unsub_msg

let next_msg_start_cb c sub timeout_time =
  CurrentSyncOperation.start
    c.cur_sync_op
    ~id:(Obj.magic sub)
    ~timeout_time
    ~signal_timeout:(fun () -> Subscription.signal_timeout sub)

let next_msg_finish_cb c sub =
  CurrentSyncOperation.finish
    c.cur_sync_op
    ~id:(Obj.magic sub)

let subscribe c ?group ?callback subject =
  if is_closed c then
    failwith "connection closed";
  let sub = Subscriptions.subscribe
      c.subscriptions subject group callback
      (unsubscribe_cb c)
      (Subscriptions.unsubscribe c.subscriptions)
      (next_msg_start_cb c)
      (next_msg_finish_cb c)
  in
  let sub_msg = ClientMessage.Sub
      (Sub.make ~subject ?group ~sid:(string_of_int (Subscription.sid sub)) ())
  in
  send_msg c sub_msg;
  sub

let publish c subject payload =
  if is_closed c then
    failwith "connection closed";
  let pub_msg = ClientMessage.Pub (Pub.make ~subject ~payload ()) in
  send_msg c pub_msg

let drain c =
  if is_closed c then
    failwith "connection closed";

  Subscriptions.iter c.subscriptions
    begin fun sub ->
      let unsub_msg = ClientMessage.UnSub
          (UnSub.make ~sid:(string_of_int (Subscription.sid sub)) ())
      in
      send_msg c unsub_msg
    end;
  flush c;
  close c
