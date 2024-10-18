(** The NATS client module. *)

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

(** The module to track unacknowledged PINGs and manage their
    acknowledgements. *)
module PingPongTracker = struct
  (** The type representing the possible outcomes of a PING. *)
  type ping_result =
    | Acknowledged
    (** The PING has been successfully acknowledged with a PONG. *)
    | TimedOut
    (** The PING has timed out without receiving a PONG. *)

  (** The type used to represent an individual PING that is awaiting
      acknowledgement. *)
  type ping = {
    mutable result : ping_result option;
    (** The result of the PING, if available. *)
    mutex : Mutex.t;
    (** A mutex to protect access to the PING's result. *)
    condition : Condition.t;
    (** A condition variable to signal the result availability. *)
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

  (** [resolve ping result] resolves [ping] with the specified [result], making
      the result available and notifying any threads that are waiting on the
      PING acknowledgement. *)
  let resolve ping result =
    Mutex.protect ping.mutex
      begin fun () ->
        ping.result <- Some result;
        Condition.signal ping.condition
      end

  (** The type representing the tracker for managing unacknowledged PINGs. *)
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

(** The module to manage the state of the current synchronous operation,
    ensuring that only one synchronous operation is active at a time.  It also
    handles timeouts by allowing a function to be called when the operation
    exceeds its allotted time. *)
module CurrentSyncOperation = struct
  (** The type representing a synchronous operation. *)
  type sync_op = {
    thread_id : int;
    (** The ID of the thread that started the synchronous operation. *)
    timeout_time : float option;
    (** An optional timeout time as an absolute Unix timestamp (in seconds). *)
    signal_timeout : unit -> unit;
    (** A function to call when the operation times out. *)
  }

  (** The type representing the state of the current synchronous operation. *)
  type t = {
    mutable cur_sync_op : sync_op option;
    (** The current synchronous operation, if any. *)
    mutex : Mutex.t;
    (** A mutex to protect access to [cur_sync_op]. *)
  }

  (** [create ()] creates a new instance of the current synchronous operation
      state. *)
  let create () = {
    cur_sync_op = None;
    mutex       = Mutex.create ();
  }

  (** [start t ~timeout_time ~signal_timeout] starts a new synchronous
      operation.  The operation is associated with the calling thread and has
      an optional [timeout_time].

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

  (** [finish t] finishes the current synchronous operation.  It ensures that
      the calling thread matches the thread that started the operation.

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
    within a synchronous operation context.  It ensures that the operation is
    properly started and finished, even if [f] raises an exception.

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

(** The module for managing active NATS subscriptions. *)
module Subscriptions = struct
  (** The type representing a collection of active NATS subscriptions. *)
  type t = {
    mutable ssid : int;
    (** A counter for generating unique subscription identifiers. *)
    subs : (int, Subscription.t) Hashtbl.t;
    (** A hashtable storing active subscriptions by their identifier. *)
    mutex : Mutex.t;
    (** A mutex to ensure thread-safe access to the subscription collection. *)
  }

  (** [create ()] creates a new, empty collection of active subscriptions. *)
  let create () = {
    ssid  = 0;
    subs  = Hashtbl.create 32;
    mutex = Mutex.create ();
  }

  (** [iter t f] applies the function [f] to each subscription in the
      collection [t]. *)
  let iter t f =
    Mutex.protect t.mutex
      (fun () -> Hashtbl.iter (fun _sid sub -> f sub) t.subs)

(** [add ?schedule_message_handling ?sync_op_started ?sync_op_finished ~unsubscribe ~remove_subscription t subject group callback]
    adds a new subscription to the collection [t] with the specified
    parameters.

    Returns the newly created subscription. *)
  let add
      ?schedule_message_handling
      ?sync_op_started ?sync_op_finished
      ~flush ~unsubscribe ~remove_subscription
      t subject group callback
    =
    Mutex.protect t.mutex
      begin fun () ->
        t.ssid <- t.ssid + 1;
        let sub = Subscription.create
            t.ssid subject group callback
            ?schedule_message_handling
            ?sync_op_started ?sync_op_finished
            ~flush ~unsubscribe ~remove_subscription
        in
        Hashtbl.add t.subs (Subscription.sid sub) sub;
        sub
      end

  (** [remove t sub] removes the given subscription [sub] from the collection
      [t]. *)
  let remove t sub =
    Mutex.protect t.mutex
      (fun () -> Hashtbl.remove t.subs (Subscription.sid sub))

  (** [remove_all t] removes all subscriptions from the collection [t]. *)
  let remove_all t =
    Mutex.protect t.mutex
      (fun () -> Hashtbl.clear t.subs)

  (** [handle_msg t msg] handles an incoming message [msg] by finding the
      corresponding subscription in the collection [t] and invoking its message
      handler.  If the subscription is not found, the message is ignored. *)
  let handle_msg t msg =
    let sub = Mutex.protect t.mutex
        (fun () -> Hashtbl.find_opt t.subs msg.Message.sid)
    in
    match sub with
    | Some sub -> Subscription.handle_msg sub msg
    | None     -> ()
end

(** The module for managing pending requests and their results. *)
module PendingRequest = struct
  (** The type representing results of a pending request. *)
  type result =
    | Response of Message.t  (** A successful response containing a message. *)
    | TimeOut                (** The request timed out without a response. *)

  (** The type representing a pending request that waits for a result. *)
  type t = {
    inbox : string;
    (** The inbox subject for receiving the response. *)
    mutable result : result option;
    (** The result of the request, if available. *)
    mutex : Mutex.t;
    (** A mutex to protect access to the result. *)
    condition : Condition.t;
    (** A condition variable to signal when the result becomes available. *)
  }

  (** [create inbox] creates a new pending request with the specified [inbox]
      subject. *)
  let create inbox = {
    inbox;
    result    = None;
    mutex     = Mutex.create ();
    condition = Condition.create ();
  }

  (** [wait t] blocks the current thread until the result of the pending
      request [t] is available.  Once the result becomes available, it returns
      the [result]. *)
  let wait t =
    Mutex.protect t.mutex
      begin fun () ->
        while Option.is_none t.result do
          Condition.wait t.condition t.mutex
        done;
        Option.get t.result
      end

  (** [resolve t result] resolves the pending request [t] with the given
      [result], making the result available and notifying any threads waiting
      on it. *)
  let resolve t result =
    Mutex.protect t.mutex
      begin fun () ->
        t.result <- Some result;
        Condition.signal t.condition
      end
end

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
  mutex : Mutex.t;
  (** A mutex for synchronizing access to the connection state. *)
  mutable state : state;
  (** The client state. *)
  sock : file_descr;
  (** The socket file descriptor for the connection. *)
  in_buffer : Bytes.t;
  (** A buffer for reading incoming data. *)
  reader : Reader.t;
  (** The reader for parsing incoming messages. *)
  serializer : Faraday.t;
  (** The serializer for sending outgoing messages. *)
  nuid : Nuid.State.t;
  (* The state for generating unique subject identifiers. *)
  inbox_prefix : string;
  (** The prefix used for generating inbox subjects. *)
  resp_sub_prefix : string;
  (** The prefix for response subscription subjects. *)
  subscriptions : Subscriptions.t;
  (** The subscription manager for handling active subscriptions. *)
  cur_sync_op : CurrentSyncOperation.t;
  (** The current synchronous operation being executed. *)
  mutable cur_request : PendingRequest.t option;
  (** The current pending request, if any. *)
  ping_pongs : PingPongTracker.t;
  (** Tracks unacknowledged PINGs. *)
  ping_interval : float;
  (** The interval (in seconds) at which PING messages are sent. *)
  mutable next_ping_time : float;
  (** The time when the next PING should be sent. *)
  mutable io_thread : Thread.t;
  (** The thread handling network I/O operations. *)
  mutable msg_thread : Thread.t;
  (** The thread handling message processing. *)
  msg_queue : Subscription.t SyncQueue.t;
  (** A queue of pending subscription messages. *)
  error_cb : error_callback;
  (** A callback function to handle errors encountered during operation. *)
}

let new_inbox c =
  Printf.sprintf "%s.%s" c.inbox_prefix (Nuid.State.next c.nuid)

(** [state c] returns the state of the client [c]. *)
let state c =
  Mutex.protect c.mutex (fun () -> c.state)

(** [send_msg c msg] sends a message [msg] to the server using the client [c].

    Raises [Failure] if the connection is closed. *)
let send_msg c msg =
  if state c = Closed then
    failwith "connection closed";

  Mutex.protect c.mutex
    (fun () -> serialize_client_message c.serializer msg)

(** [send_ping c] sends a PING message using the client [c] and returns the
    corresponding PING tracker object.  This can be used to wait for the PONG
    response. *)
let send_ping c =
  let ping = PingPongTracker.ping c.ping_pongs in
  send_msg c ClientMessage.Ping;
  ping

let flush ?timeout c =
  if state c = Closed then
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
  Mutex.protect c.mutex (fun () -> c.state <- Closing);
  SyncQueue.signal_interrupt c.msg_queue;
  Thread.join c.io_thread;
  Thread.join c.msg_thread;
  Mutex.protect c.mutex (fun () -> c.state <- Closed)

(** [read_client_msg ?timeout c] reads the next message from the server for the
    client [c].  If a [timeout] is provided, the operation will fail if the
    timeout is exceeded.

    Raises [Failure] if the connection is closed or if there is an error
    parsing the message.*)
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

(** [io_loop c] runs the I/O loop for the client [c], managing both incoming
    and outgoing messages. *)
let rec io_loop c =
  let exception Close in
  let select_timeout = 0.05 in

  try
    Fun.protect
      ~finally:begin fun () ->
        Mutex.protect c.mutex
          begin fun () ->
            Subscriptions.iter c.subscriptions Subscription.close;
            Subscriptions.remove_all c.subscriptions;
            shutdown c.sock SHUTDOWN_ALL;
            Unix.close c.sock;
            Faraday.close c.serializer;
            ignore (Faraday.drain c.serializer)
          end
      end
      begin fun () ->
        while state c = Connected do
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
            c.next_ping_time <- c.next_ping_time +. c.ping_interval
          end
        done
      end
  with
  | Close -> ()
  | exn -> c.error_cb exn

(** [dispatch_message c msg] handles an incoming message [msg] from the server
    for the client [c]. *)
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

(** [msg_loop c] processes incoming messages for the client [c].

    This loop continues running as long as the client is active, fetching
    messages from the queue and invoking the appropriate subscription
    callbacks  *)
let msg_loop c =
  let should_stop () = state c <> Connected in

  while state c = Connected do
    match SyncQueue.peek ~interrupt_cond:should_stop c.msg_queue with
    | Some sub ->
      begin match Subscription.next_msg_internal sub with
        | Some msg ->
          let callback = Option.get @@ Subscription.callback sub in
          callback msg
        | None -> ()  (* message queue was cleared *)
      end;
      (* Remove the processed subscription from the queue. *)
      ignore @@ SyncQueue.try_get c.msg_queue
    | None -> ()  (* interrupted *)
  done

(** [default_error_callback exn] is the default callback for handling errors.
    It prints the encountered exception to standard error. *)
let default_error_callback exn =
  Printf.fprintf
    Stdlib.stderr
    "NATS: encoutered error %s\n%!" (Printexc.to_string exn)

(** [schedule_message_handling c sub ~msg] schedules a message from the
    subscription [sub] to be processed by adding it to the client's message
    queue. *)
let schedule_message_handling c sub ~msg:_msg =
  SyncQueue.put c.msg_queue sub

(** [unsubscribe c sub] sends an UNSUB message to unsubscribe the subscription
    [sub] using the client [c].  Raises [Failure] if the connection is
    closed. *)
let unsubscribe c sub =
  if state c = Closed then
    failwith "connection closed";

  let unsub_msg = ClientMessage.UnSub
      (UnSub.make
         ~sid:(string_of_int (Subscription.sid sub))
         ?max_msgs:(Subscription.max_msgs sub)
         ())
  in
  send_msg c unsub_msg

(** [sync_sub_op_started c sub ~timeout_time] registers the start of a
    synchronous operation for the subscription [sub], with an optional
    timeout. *)
let sync_sub_op_started c sub ~timeout_time =
  CurrentSyncOperation.start
    c.cur_sync_op
    ~timeout_time
    ~signal_timeout:(fun () -> Subscription.signal_timeout sub)

(** [sync_sub_op_finished c sub] marks the completion of a synchronous
    operation for the subscription [sub].*)
let sync_sub_op_finished c _sub =
  CurrentSyncOperation.finish c.cur_sync_op

let subscribe c ?group ?callback subject =
  if state c = Closed then
    failwith "connection closed";

  let schedule_message_handling =
    match callback with
    | Some _ -> Some (schedule_message_handling c)
    | None   -> None
  in
  let sub = Subscriptions.add
      c.subscriptions subject group callback
      ?schedule_message_handling
      ~sync_op_started:(sync_sub_op_started c)
      ~sync_op_finished:(sync_sub_op_finished c)
      ~flush:(fun _sub ~timeout -> flush c ?timeout)
      ~unsubscribe:(unsubscribe c)
      ~remove_subscription:(Subscriptions.remove c.subscriptions)
  in
  let sub_msg = ClientMessage.Sub
      (Sub.make ~subject ?group ~sid:(string_of_int (Subscription.sid sub)) ())
  in
  send_msg c sub_msg;
  sub

let publish c ?reply subject payload =
  if state c = Closed then
    failwith "connection closed";
  let pub_msg = ClientMessage.Pub (Pub.make ~subject ?reply ~payload ()) in
  send_msg c pub_msg

let request c ?timeout subject payload =
  if state c = Closed then
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
  if state c = Closed then
    failwith "connection closed";

  Subscriptions.iter c.subscriptions
    begin fun sub ->
      let unsub_msg = ClientMessage.UnSub
          (UnSub.make ~sid:(string_of_int (Subscription.sid sub)) ())
      in
      send_msg c unsub_msg
    end;
  flush c;
  ignore (SyncQueue.join c.msg_queue);
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
    mutex           = Mutex.create ();
    state           = Connected;
    sock;
    in_buffer       = Bytes.create 0x1000;
    reader          = Reader.create ();
    serializer      = Faraday.create 0x1000;
    subscriptions   = Subscriptions.create ();
    resp_sub_prefix = Printf.sprintf "%s.%s." inbox_prefix (Nuid.State.next nuid);
    cur_request     = None;
    io_thread       = Thread.self ();
    msg_thread      = Thread.self ();
    msg_queue       = SyncQueue.create ();
    ping_pongs      = PingPongTracker.create ();
    ping_interval;
    next_ping_time  = Unix.gettimeofday () +. ping_interval;
    cur_sync_op     = CurrentSyncOperation.create ();
    error_cb        = error_cb;
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
            | Some req when msg.subject = req.inbox ->
              PendingRequest.(resolve req (Response msg))
            | _ -> ()
          end
      end
  in

  conn.io_thread  <- Thread.create io_loop conn;
  conn.msg_thread <- Thread.create msg_loop conn;

  conn
