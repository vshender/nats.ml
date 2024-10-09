(** A NATS client module. *)

open Compat

open Unix

open Nats.Parse
open Nats.Serialize
open Nats.Protocol

let client_name    = "nats.ml-unix"
let client_lang    = "ocaml"
let client_version = "0.0.1-dev"

let default_inbox_prefix = "_INBOX"

let default_ping_interval = 120.  (* in seconds *)

type callback = Subscription.callback
type error_callback = exn -> unit

(** A module to track unacknowledged PINGs and manage their acknowledgement. *)
module PingPongTracker = struct
  (** A type representing the possible outcomes of a PING. *)
  type ping_result =
    | Acknowledged
    (** The PING has been successfully acknowledged with a PONG. *)
    | TimedOut
    (** The PING has timed out. *)

  (** A type used to track individual PINGs. *)
  type ping = {
    mutable result : ping_result option;
    (** The result of the PING. *)
    mutex : Mutex.t;
    (** A mutex to protect access to the result. *)
    condition : Condition.t;
    (** A condition variable to signal result availability. *)
  }

  (** [wait ping] blocks the current thread until the result of [ping] is
      available. *)
  let wait ping =
    Mutex.protect ping.mutex
      begin fun () ->
        while Option.is_none ping.result do
          Condition.wait ping.condition ping.mutex
        done;
        Option.get ping.result
      end

  (** [resolve ping result] resolves [ping] with the specified [result], *)
  let resolve ping result =
    Mutex.protect ping.mutex
      begin fun () ->
        ping.result <- Some result;
        Condition.signal ping.condition
      end

  (** A type representing PING-PONG trackers. *)
  type t = {
    pings : ping Queue.t;  (** A queue to hold unacknowledged PINGs. *)
    mutex : Mutex.t;       (** A mutex to protect access to the PINGs queue. *)
  }

  (** [create ()] creates a new PING-PONG tracker. *)
  let create () = {
    pings = Queue.create ();
    mutex = Mutex.create ();
  }

  (** [ping t] creates a new unacknowledged PING and adds it to the PINGs
      queue of the tracker [t].  Returns the newly created PING object, which
      can be used to wait for its result. *)
  let ping t =
    let ping = {
      result    = None;
      mutex     = Mutex.create ();
      condition = Condition.create ();
    } in
    Mutex.protect t.mutex
      (fun () -> Queue.add ping t.pings);
    ping

  (** [pong t] acknowledges the first unacknowledged PING in the PINGs queue of
      the tracker [t].  It removes the PING from the queue and resolves it as
      [Acknowledged], signaling any thread waiting for the PING to be
      acknowledged. *)
  let pong t =
    Mutex.protect t.mutex
      begin fun () ->
        match Queue.take_opt t.pings with
        | Some ping -> resolve ping Acknowledged
        | None      -> assert false
      end
end

(** A module to manage the state of the current synchronous operation, ensuring
    that only one synchronous operation is active at a time.  It also handles
    timeouts by allowing a function to be called when the operation exceeds its
    allotted time. *)
module CurrentSyncOperation = struct
  (** A type representing a synchronous operation. *)
  type sync_op = {
    thread_id : int;
    (** The ID of the thread that started the operation. *)
    timeout_time : float option;
    (** Optional timeout time as an absolute Unix timestamp. *)
    signal_timeout : unit -> unit;
    (** A function to call when the operation times out. *)
  }

  (** A type representing the state of the current synchronous operation. *)
  type t = {
    mutable cur_sync_op : sync_op option;
    (** The current synchronous operation, if any. *)
    mutex : Mutex.t;
    (** A mutex to protect access to [cur_sync_op]. *)
  }

  (** [create ()] initializes a new current synchronous operation state. *)
  let create () = {
    cur_sync_op = None;
    mutex       = Mutex.create ();
  }

  (** [start t ~timeout_time ~signal_timeout] begins a new synchronous
      operation.

      Raises [Failure] if another synchronous operation is already running. *)
  let start t ~timeout_time ~signal_timeout =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | None ->
          t.cur_sync_op <- Some {
              thread_id = Thread.id @@ Thread.self ();
              timeout_time;
              signal_timeout
            }
        | Some _ -> failwith "another sync operation is running"
      end

  (** [finish t] ends the current synchronous operation.

      Raises [Failure] if no operation is running or if the operation was
      started by a different thread. *)
  let finish t =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | Some { thread_id = tid; _ } when tid = Thread.id @@ Thread.self () ->
          t.cur_sync_op <- None
        | Some _ -> failwith "another sync operation is running"
        | None   -> failwith "no sync operation is running"
      end

(** [with_sync_op t ~timeout_time ~signal_timeout f] executes function [f]
    within a synchronous operation context.

    This function ensures that [start] and [finish] are properly called, even
    if [f] raises an exception.

    Raises [Failure] if another synchronous operation is already running. *)
  let with_sync_op t ~timeout_time ~signal_timeout f =
    start t ~timeout_time ~signal_timeout;
    Fun.protect
      ~finally:(fun () -> finish t)
      f

  (** [check_timeout t] checks if the current synchronous operation has
      exceeded its timeout, and if so, calls the [signal_timeout] function
      associated with it.

      This function should be called periodically to enforce timeouts on
      synchronous operations. *)
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

module PendingRequest = struct
  type result =
    | Response of Message.t
    | TimeOut

  type t = {
    inbox          : string;
    mutable result : result option;
    mutex          : Mutex.t;
    condition      : Condition.t;
  }

  let create inbox = {
    inbox;
    result    = None;
    mutex     = Mutex.create ();
    condition = Condition.create ();
  }

  let wait t =
    Mutex.protect t.mutex
      begin fun () ->
        while Option.is_none t.result do
          Condition.wait t.condition t.mutex
        done;
        Option.get t.result
      end

  let resolve t result =
    Mutex.protect t.mutex
      begin fun () ->
        t.result <- Some result;
        Condition.signal t.condition
      end
end

type t = {
  mutex                  : Mutex.t;
  sock                   : file_descr;
  in_buffer              : Bytes.t;
  reader                 : Reader.t;
  serializer             : Faraday.t;
  subscriptions          : Subscriptions.t;
  resp_sub_prefix        : string;
  mutable cur_request    : PendingRequest.t option;
  mutable running        : bool;
  mutable io_thread      : Thread.t;
  ping_pongs             : PingPongTracker.t;
  ping_interval          : float;
  mutable next_ping_time : float;
  cur_sync_op            : CurrentSyncOperation.t;
  error_cb               : error_callback;
  inbox_prefix           : string;
  nuid                   : Nuid.State.t;
}

let new_inbox c =
  Printf.sprintf "%s.%s" c.inbox_prefix (Nuid.State.next c.nuid)

let is_running c =
  c.running

let is_closed c =
  not c.running

let send_msg c msg =
  if is_closed c then
    failwith "connection closed";

  Mutex.protect c.mutex
    (fun () -> serialize_client_message c.serializer msg)

let send_ping c =
  let ping = PingPongTracker.ping c.ping_pongs in
  send_msg c ClientMessage.Ping;
  ping

let flush ?timeout c =
  if is_closed c then
    failwith "connection closed";

  let timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

  let ping = send_ping c in
  CurrentSyncOperation.with_sync_op
    c.cur_sync_op
    ~timeout_time:timeout_time
    ~signal_timeout:(fun () ->
        PingPongTracker.resolve ping PingPongTracker.TimedOut)
    begin fun () ->
      match PingPongTracker.wait ping with
      | Acknowledged -> ()
      | TimedOut     -> failwith "timeout"  (* TODO: better errors *)
    end

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
          let iovec = Mutex.protect c.mutex
              begin fun () ->
                match Faraday.operation c.serializer with
                | `Writev (iovec :: _) -> Some iovec
                | _                   -> None
              end
          in

          let (readable, writeable, _) = select
              [c.sock]
              (if Option.is_some iovec then [c.sock] else [])
              []
              select_timeout
          in

          (* Read incoming data *)
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
          if List.length writeable > 0 then
            iovec |> Option.iter
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

          (* PING/PONG *)
          if Unix.gettimeofday () >= c.next_ping_time then begin
            ignore @@ send_ping c;
            Mutex.protect c.mutex
              (fun () -> c.next_ping_time <- c.next_ping_time +. c.ping_interval)
          end
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
    PingPongTracker.pong c.ping_pongs
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
    ~timeout_time
    ~signal_timeout:(fun () -> Subscription.signal_timeout sub)

let next_msg_finish_cb c _sub =
  CurrentSyncOperation.finish c.cur_sync_op

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

let publish c ?reply subject payload =
  if is_closed c then
    failwith "connection closed";
  let pub_msg = ClientMessage.Pub (Pub.make ~subject ?reply ~payload ()) in
  send_msg c pub_msg

let request c ?timeout subject payload =
  if is_closed c then
    failwith "connection closed";

  let timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

  let resp_subject = c.resp_sub_prefix ^ (Nuid.State.next c.nuid) in
  let req = PendingRequest.create resp_subject in

  CurrentSyncOperation.with_sync_op
    c.cur_sync_op
    ~timeout_time
    ~signal_timeout:(fun () -> PendingRequest.(resolve req TimeOut))
    begin fun () ->
      Mutex.protect c.mutex (fun () -> c.cur_request <- Some req);
      Fun.protect
        ~finally:(fun () -> Mutex.protect c.mutex (fun () -> c.cur_request <- None))
        begin fun () ->
          publish c ~reply:resp_subject subject payload;

          match PendingRequest.wait req with
          | Response msg -> Some msg
          | TimeOut      -> None
        end
    end

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

let connect
    ?(url = "nats://127.0.0.1:4222")
    ?(name = client_name)
    ?(verbose = false)
    ?(pedantic = false)
    ?connect_timeout
    ?(keepalive = false)  (* ??? *)
    ?(ping_interval = default_ping_interval)
    ?(error_cb = default_error_callback)
    ?(inbox_prefix = default_inbox_prefix)
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

  let nuid = Nuid.State.create () in

  let conn = {
    mutex            = Mutex.create ();
    sock;
    in_buffer        = Bytes.create 0x1000;
    reader           = Reader.create ();
    serializer       = Faraday.create 0x1000;
    subscriptions    = Subscriptions.create ();
    resp_sub_prefix  = Printf.sprintf "%s.%s." inbox_prefix (Nuid.State.next nuid);
    cur_request      = None;
    running          = true;
    io_thread        = Thread.self ();
    ping_pongs       = PingPongTracker.create ();
    ping_interval;
    next_ping_time   = Unix.gettimeofday () +. ping_interval;
    cur_sync_op      = CurrentSyncOperation.create ();
    error_cb         = error_cb;
    inbox_prefix;
    nuid;
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
         ~name
         ~lang:client_lang
         ~protocol:0
         ~version:client_version
         ~verbose
         ~pedantic
         ~tls_required:false
         ~echo:true
         ~no_responders:false
         ~headers:false
         ())
  in
  send_msg conn connect;

  let _ = subscribe conn (conn.resp_sub_prefix ^ "*")
      ~callback:begin fun msg ->
        Mutex.protect conn.mutex
          begin fun () ->
            let req = conn.cur_request in
            match req with
            | Some req ->
              if msg.subject = req.inbox then
                PendingRequest.(resolve req (Response msg))
            | None ->
              ()
          end
      end
  in

  conn.io_thread <- Thread.create io_loop conn;

  conn
