(** The NATS client module. *)

open Compat

open Unix

open Nats.Parse
open Nats.Serialize
open Nats.Protocol

open Errors

let client_name    = "nats.ml-unix"
let client_lang    = "ocaml"
let client_version = "0.0.1-dev"

let default_hostname = "127.0.0.1"
let default_port     = 4222

let default_url = Printf.sprintf "nats://%s:%d" default_hostname default_port

let default_inbox_prefix = "_INBOX"

let default_ping_interval = 120.       (* in seconds *)
let default_max_pings_outstanding = 2

type callback = Subscription.callback

(** The module to track unacknowledged PINGs and manage their
    acknowledgements. *)
module PingPongTracker = struct
  (** The type representing the possible outcomes of a PING. *)
  type ping_result =
    | Acknowledged
    (** The PING has been successfully acknowledged with a PONG. *)
    | Error of Errors.t
    (** An error occurred while waiting for a PONG. *)

  (** The type used to represent an individual PING that is awaiting
      acknowledgement. *)
  type ping = {
    mutable result : ping_result option;
    (** The result of the PING, if available. *)
    mutex : Mutex.t;
    (** A mutex to protect access to the PING's result. *)
    resolved : Condition.t;
    (** A condition variable to signal when the result is available. *)
  }

  (** [wait ping] blocks the current thread until the result of [ping] is
      available.  Returns the result. *)
  let wait ping =
    Mutex.protect ping.mutex
      begin fun () ->
        while Option.is_none ping.result do
          Condition.wait ping.resolved ping.mutex
        done;
        Option.get ping.result
      end

  (** [resolve ping result] resolves [ping] with the specified [result],
      notifying any threads that are waiting on the PING acknowledgement. *)
  let resolve ping result =
    Mutex.protect ping.mutex
      begin fun () ->
        ping.result <- Some result;
        Condition.signal ping.resolved
      end

  (** The type representing a tracker for managing unacknowledged PINGs. *)
  type t = {
    pings : ping Queue.t;  (** A queue of unacknowledged PINGs. *)
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
      result   = None;
      mutex    = Mutex.create ();
      resolved = Condition.create ();
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
        | None      -> ()
      end
end

(** The module to manage the state of the current synchronous operation,
    ensuring that only one synchronous operation is active at a time.  It also
    provides the ability to handle errors and timeouts by calling a function
    that interrupts the operation. *)
module CurrentSyncOperation = struct
  (** The type representing a synchronous operation. *)
  type sync_op = {
    thread_id : int;
    (** The ID of the thread that started the synchronous operation. *)
    signal_interrupt : Errors.t -> unit;
    (** A function to interrupt the synchronous operation in case of failure
        (e.g., connection closed or timed out). *)
    timeout_time : float option;
    (** An optional timeout time as an absolute Unix timestamp (in seconds). *)
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

  (** [start t ~signal_interrupt ~timeout_time] starts a new synchronous
      operation.  The operation is associated with the calling thread and has
      an optional [timeout_time].

      Raises [Failure] if another synchronous operation is already running. *)
  let start t ~signal_interrupt ~timeout_time =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | None ->
          t.cur_sync_op <- Some {
              thread_id = Thread.id @@ Thread.self ();
              signal_interrupt;
              timeout_time;
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

(** [with_sync_op t ~signal_timeout ~timeout_time f] executes the function [f]
    within a synchronous operation context.  It ensures that the operation is
    properly started and finished, even if [f] raises an exception.

    Raises [Failure] if another synchronous operation is already running. *)
  let with_sync_op t ~signal_interrupt ~timeout_time f =
    start t ~signal_interrupt ~timeout_time;
    Fun.protect
      ~finally:(fun () -> finish t)
      f

  (** [interrupt t err] interrupts the current synchronous operation with the
      specified error [err]. *)
  let interrupt t err =
    Mutex.protect t.mutex
      begin fun () ->
        t.cur_sync_op
        |> Option.iter (fun { signal_interrupt; _ } -> signal_interrupt err)
      end

  (** [check_timeout t] checks if the current synchronous operation has
      exceeded its timeout, and if so, interrupts it with the [Timeout] error.

      This function should be called periodically to enforce timeouts on
      synchronous operations. *)
  let check_timeout t =
    Mutex.protect t.mutex
      begin fun () ->
        match t.cur_sync_op with
        | Some { timeout_time = Some ttime; signal_interrupt; _ } ->
          if gettimeofday () >= ttime then signal_interrupt Timeout
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

(** [add ?schedule_message_handling ?sync_op_started ?sync_op_finished ~flush ~unsubscribe ~remove_subscription t subject group callback]
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
  (** The type representing results of pending requests. *)
  type result =
    | Response of Message.t
    (** A successful response containing a message. *)
    | Error of Errors.t
    (** An error occurred while waiting for the response. *)

  (** The type representing a pending request that waits for a result. *)
  type t = {
    inbox : string;
    (** The inbox subject for receiving the response. *)
    mutable result : result option;
    (** The result of the request, if available. *)
    mutex : Mutex.t;
    (** A mutex to protect access to the result. *)
    resolved : Condition.t;
    (** A condition variable to signal when the result becomes available. *)
  }

  (** [create inbox] creates a new pending request with the specified [inbox]
      subject. *)
  let create inbox = {
    inbox;
    result   = None;
    mutex    = Mutex.create ();
    resolved = Condition.create ();
  }

  (** [wait t] blocks the current thread until the result of the pending
      request [t] becomes available.  Once the result is available, the
      function returns it. *)
  let wait t =
    Mutex.protect t.mutex
      begin fun () ->
        while Option.is_none t.result do
          Condition.wait t.resolved t.mutex
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
        Condition.signal t.resolved
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
  options : options;
  (** The client configuration options. *)
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
  mutable next_ping_time : float;
  (** The time when the next PING should be sent. *)
  mutable pings_outstanding : int;
  (** The number of pending PING commands that are awaiting a response. *)
  mutable io_thread : Thread.t;
  (** The thread handling network I/O operations. *)
  mutable msg_thread : Thread.t;
  (** The thread handling message processing. *)
  msg_queue : Subscription.t SyncQueue.t;
  (** A queue of pending subscription messages. *)
  mutable err : Errors.t option;
  (** The last error that occurred while using the client. *)
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
  ping_interval : float;
  (** The interval (in seconds) at which the client will be sending PING
      messages to the server.  Defaults to 2m.  *)
  max_pings_outstanding : int;
  (** The maximum number of pending PING commands that can be awaiting a
      response before raising a [StaleConnection] error. *)
  connect_timeout : float option;
  (** The timeout for a connection operation to complete. *)
  closed_cb : conn_callback;
  (** A callback function that is called when the client is no longer connected
      to a server. *)
  error_cb : error_callback;
  (** A callback function to report asynchronous errors. *)
}

and conn_callback = t -> unit

and error_callback = t -> Errors.t -> unit

(** [state c] returns the state of the client [c]. *)
let state c =
  Mutex.protect c.mutex (fun () -> c.state)

let is_closed c =
  state c = Closed

let last_error c =
  Mutex.protect c.mutex (fun () -> c.err)

(** [set_last_error c err] sets the last error encountered via the
    connection. *)
let set_last_error c err =
  Mutex.protect c.mutex (fun () -> c.err <- Some err)

let new_inbox c =
  Printf.sprintf "%s.%s" c.inbox_prefix (Nuid.State.next c.nuid)

(** [send_msg_direct c msg] serializes the given message [msg] and sends it
    directly to the server by writing it to the client's socket.  This function
    blocks the current thread until the message is successfully sent. *)
let send_msg_direct c msg =
  let serializer = Faraday.create 0x100 in
  serialize_client_message serializer msg;
  let data = Faraday.serialize_to_bigstring serializer in
  try
    ignore @@ write_bigarray c.sock data 0 (Bigstringaf.length data)
  with Unix_error (EPIPE, _, _) ->
    Errors.nats_error ConnectionLost

(** [send_msg c msg] serializes the given message [msg] and enqueues it for
    sending by the I/O thread, which ensures asynchronous, non-blocking
    delivery of messages to the server.

    Raises [NatsError ConnectionClosed] if the connection is closed. *)
let send_msg c msg =
  if is_closed c then
    Errors.nats_error ConnectionClosed;

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
  if is_closed c then
    Errors.nats_error ConnectionClosed;

  let timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

  let ping = send_ping c in
  CurrentSyncOperation.with_sync_op
    c.cur_sync_op
    ~signal_interrupt:(fun err -> PingPongTracker.resolve ping @@ Error err)
    ~timeout_time
    begin fun () ->
      match PingPongTracker.wait ping with
      | Acknowledged -> ()
      | Error err    -> Errors.nats_error err
    end

let close c =
  if is_closed c then
    Errors.nats_error ConnectionClosed;

  Mutex.protect c.mutex (fun () -> c.state <- Closing);

  SyncQueue.signal_interrupt c.msg_queue;
  Thread.join c.io_thread;
  Thread.join c.msg_thread;

  Mutex.protect c.mutex (fun () -> c.state <- Closed);

  c.options.closed_cb c

(** [close_from_io_thread c] closes the connection to the NATS server.

    This function is intended to be called from the I/O thread in case of an
    error.*)
let close_from_io_thread c =
  Mutex.protect c.mutex (fun () -> c.state <- Closing);

  SyncQueue.signal_interrupt c.msg_queue;
  Thread.join c.msg_thread;

  Mutex.protect c.mutex (fun () -> c.state <- Closed);

  c.options.closed_cb c

(** [process_op_err c err] handles errors which occured while reading or
    parsing the protocol. *)
let process_op_err c err =
  set_last_error c err;
  close_from_io_thread c;
  CurrentSyncOperation.interrupt c.cur_sync_op err

(** [process_err c err_msg] handles error messages from the server. *)
let process_err c err_msg =
  let err = parse_server_err_msg err_msg in
  match err with
  | StaleConnection | MaxConnectionsExceeded ->
    process_op_err c err
  | InvalidSubject | PermissionsViolation _ ->
    set_last_error c err;
    c.options.error_cb c err
  | _ ->
    set_last_error c err;
    CurrentSyncOperation.interrupt c.cur_sync_op err;
    close_from_io_thread c

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
      msg
    | Reader.Need_input ->
      Socket.read
        ?timeout:(remaining_time ())
        c.sock
        c.in_buffer 0 (Bytes.length c.in_buffer)
      |> begin function
        | 0 ->
          Errors.nats_error ConnectionLost;
        | n ->
          Reader.feed c.reader c.in_buffer 0 n;
          loop ()
      end
    | Reader.Parse_error (_, e) ->
      Errors.nats_error (ProtocolError e)
  in
  try
    loop ()
  with Socket.Timed_out ->
    Errors.nats_error Timeout

(** [io_loop c] runs the I/O loop for the client [c], managing both incoming
    and outgoing messages. *)
let rec io_loop c =
  let select_timeout = 0.05 in
  let exception Break of Errors.t in
  try
    Fun.protect
      ~finally:begin fun () ->
        Subscriptions.iter c.subscriptions Subscription.close;
        Subscriptions.remove_all c.subscriptions;
        begin
          try
            shutdown c.sock SHUTDOWN_ALL
          with Unix_error (ENOTCONN, _, _) ->
            ()
        end;
        Unix.close c.sock;
        Mutex.protect c.mutex
          begin fun () ->
            Faraday.close c.serializer;
            ignore @@ Faraday.drain c.serializer
          end
      end
      begin fun () ->
        while state c = Connected do
          let iovec = Mutex.protect c.mutex
              begin fun () ->
                match Faraday.operation c.serializer with
                | `Writev (iovec :: _) -> Some iovec
                | _                    -> None
              end
          in

          let (readable, writeable, _) = select
              [c.sock]
              (if Option.is_some iovec then [c.sock] else [])
              []
              select_timeout
          in

          (* Read incoming data. *)
          if List.length readable > 0 then begin
            match read c.sock c.in_buffer 0 (Bytes.length c.in_buffer) with
            | 0 ->
              raise @@ Break ConnectionLost
            | n ->
              Reader.feed c.reader c.in_buffer 0 n
          end;

          (* Send outgoing data. *)
          if List.length writeable > 0 then
            iovec |> Option.iter
              begin fun iovec ->
                match
                  write_bigarray
                    c.sock
                    iovec.Faraday.buffer iovec.off iovec.len
                with
                | 0 | exception Unix_error (EPIPE, _, _) ->
                  raise @@ Break ConnectionLost
                | n ->
                  Mutex.protect c.mutex
                    (fun () -> Faraday.shift c.serializer n)
              end;

          (* Process the current operation's timeout. *)
          CurrentSyncOperation.check_timeout c.cur_sync_op;

          (* Process messages. *)
          let rec msg_processing_loop () =
            let next_msg = Reader.next_msg c.reader in
            match next_msg with
            | Reader.Message msg ->
              dispatch_message c msg;
              msg_processing_loop ()
            | Reader.Need_input ->
              ()
            | Reader.Parse_error (_, e) ->
              raise @@ Break (ProtocolError e)
          in msg_processing_loop ();

          (* PING/PONG *)
          if Unix.gettimeofday () >= c.next_ping_time then begin
            c.pings_outstanding <- c.pings_outstanding + 1;
            if c.pings_outstanding >= c.options.max_pings_outstanding then
              process_op_err c StaleConnection
            else begin
              ignore @@ send_ping c;
              c.next_ping_time <- c.next_ping_time +. c.options.ping_interval
            end
          end
        done
      end
  with
  | Break err ->
    process_op_err c err
  | exn ->
    (* TODO: handle the error *)
    raise exn

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
    c.pings_outstanding <- 0;
    PingPongTracker.pong c.ping_pongs
  | ServerMessage.Info _ ->
    ()  (* TODO: handle updated info *)
  | ServerMessage.Err msg ->
    process_err c msg
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
      let q = Subscription.queue sub in
      begin match SyncQueue.try_peek q with
        | Some msg ->
          let callback = Option.get @@ Subscription.callback sub in
          begin
            try
              callback msg;
            with exn ->
              c.options.error_cb c (Errors.MessageCallbackError exn)
          end;
          ignore @@ SyncQueue.try_get q
        | None -> ()  (* message queue was cleared *)
      end;
      (* Remove the processed subscription from the queue. *)
      ignore @@ SyncQueue.try_get c.msg_queue
    | None -> ()  (* interrupted *)
  done

(** [default_error_callback] is the default callback for handling errors.
    It prints the encountered error to standard error. *)
let default_error_callback _c err =
  Printf.fprintf Stdlib.stderr "nats error: %s\n%!" (error_message err)

(** [schedule_message_handling c sub ~msg] schedules a message from the
    subscription [sub] to be processed by adding it to the client's message
    queue. *)
let schedule_message_handling c sub ~msg:_msg =
  SyncQueue.put c.msg_queue sub

(** [unsubscribe c sub] sends an UNSUB message to unsubscribe the subscription
    [sub] using the client [c].

    Raises [NatsError ConnectionClosed] if the connection is closed. *)
let unsubscribe c sub =
  if is_closed c then
    Errors.nats_error ConnectionClosed;

  let unsub_msg = ClientMessage.UnSub
      (UnSub.make
         ~sid:(string_of_int (Subscription.sid sub))
         ?max_msgs:(Subscription.max_msgs sub)
         ())
  in
  send_msg c unsub_msg

(** [sync_sub_op_started c sub ~signal_interrupt ~timeout_time] registers the
    start of a synchronous operation for the subscription [sub], with an
    optional timeout. *)
let sync_sub_op_started c _sub ~signal_interrupt ~timeout_time =
  CurrentSyncOperation.start
    c.cur_sync_op
    ~timeout_time
    ~signal_interrupt

(** [sync_sub_op_finished c sub] marks the completion of a synchronous
    operation for the subscription [sub].*)
let sync_sub_op_finished c _sub =
  CurrentSyncOperation.finish c.cur_sync_op

let subscribe c ?group ?callback subject =
  if is_closed c then
    Errors.nats_error ConnectionClosed;

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
  if is_closed c then
    Errors.nats_error ConnectionClosed;

  let pub_msg = ClientMessage.Pub (Pub.make ~subject ?reply ~payload ()) in
  send_msg c pub_msg

let request c ?timeout subject payload =
  if is_closed c then
    Errors.nats_error ConnectionClosed;

  let timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

  let resp_subject = c.resp_sub_prefix ^ (Nuid.State.next c.nuid) in
  let req = PendingRequest.create resp_subject in

  CurrentSyncOperation.with_sync_op
    c.cur_sync_op
    ~signal_interrupt:(fun err -> PendingRequest.resolve req @@ Error err)
    ~timeout_time
    begin fun () ->
      Mutex.protect c.mutex (fun () -> c.cur_request <- Some req);
      Fun.protect
        ~finally:(fun () -> Mutex.protect c.mutex (fun () -> c.cur_request <- None))
        begin fun () ->
          publish c ~reply:resp_subject subject payload;

          match PendingRequest.wait req with
          | Response msg  -> msg
          | Error err     -> Errors.nats_error err
        end
    end

let request_opt c ?timeout subject payload =
  try
    Some (request c subject payload ?timeout)
  with NatsError Timeout ->
    None

let drain ?timeout c =
  if is_closed c then
    Errors.nats_error ConnectionClosed;

  let timeout_time, timed_out =
    match timeout with
    | Some timeout ->
      let timeout_time = Unix.gettimeofday () +. timeout in
      let timed_out () = Unix.gettimeofday () > timeout_time in
      Some timeout_time, Some timed_out
    | None ->
      None, None
  in

  Subscriptions.iter c.subscriptions
    begin fun sub ->
      let unsub_msg = ClientMessage.UnSub
          (UnSub.make ~sid:(string_of_int (Subscription.sid sub)) ())
      in
      send_msg c unsub_msg
    end;

  Fun.protect
    ~finally:(fun () -> if state c = Connected then close c)
    begin fun () ->
      flush c ?timeout;

      CurrentSyncOperation.with_sync_op
        c.cur_sync_op
        ~signal_interrupt:(fun _ -> SyncQueue.signal_interrupt c.msg_queue)
        ~timeout_time
        begin fun () ->
          if not (SyncQueue.join c.msg_queue ?interrupt_cond:timed_out) then begin
            SyncQueue.clear c.msg_queue;
            Errors.nats_error Timeout
          end
        end
    end

(** [process_expected_info ?timeout c] looks for the expected first INFO
    message sent when a connection is established. *)
let process_expected_info ?timeout c =
  let info = read_client_msg c ?timeout in
  match info with
  | ServerMessage.Info info ->
    if info.Info.tls_required then
      failwith "client doesn't support TLS";
  | _ -> Errors.nats_error NoInfoReceived

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
  send_msg_direct c connect_msg;

  send_msg_direct c ClientMessage.Ping;
  let msg = read_client_msg c ?timeout:(remaining_time ()) in
  let msg =
    if c.options.verbose && msg = ServerMessage.Ok then
      read_client_msg c ?timeout:(remaining_time ())
    else
      msg
  in
  match msg with
  | ServerMessage.Pong    -> ()
  | ServerMessage.Err msg -> Errors.nats_error (parse_server_err_msg msg)
  | _                     -> Errors.nats_error (UnexpectedProtocol msg)

let connect
    ?(url = default_url)
    ?(name = client_name)
    ?(verbose = false)
    ?(pedantic = false)
    ?connect_timeout
    ?(ping_interval = default_ping_interval)
    ?(max_pings_outstanding = default_max_pings_outstanding)
    ?(closed_cb = Fun.const ())
    ?(error_cb = default_error_callback)
    ?(inbox_prefix = default_inbox_prefix)
    () =
  let options = {
    url;
    name;
    verbose;
    pedantic;
    ping_interval;
    max_pings_outstanding;
    connect_timeout;
    closed_cb;
    error_cb;
  } in

  let uri = Uri.of_string options.url in
  let hostname = Uri.host uri |> Option.value ~default:default_hostname
  and port     = Uri.port uri |> Option.value ~default:default_port in

  let remaining_time = remaining_time_fn options.connect_timeout in

  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = setsockopt sock TCP_NODELAY true in

  let addr = ADDR_INET (inet_addr_of_string hostname, port) in
  begin
    try
      Socket.connect ?timeout:(remaining_time ()) sock addr
    with Unix_error (ECONNREFUSED, _, _) ->
      Errors.nats_error NoServers
  end;

  let nuid = Nuid.State.create () in
  let conn = {
    mutex             = Mutex.create ();
    options;
    state             = Connected;
    sock;
    in_buffer         = Bytes.create 0x1000;
    reader            = Reader.create ();
    serializer        = Faraday.create 0x1000;
    nuid;
    inbox_prefix;
    resp_sub_prefix   = Printf.sprintf
        "%s.%s." inbox_prefix (Nuid.State.next nuid);
    subscriptions     = Subscriptions.create ();
    cur_sync_op       = CurrentSyncOperation.create ();
    cur_request       = None;
    ping_pongs        = PingPongTracker.create ();
    next_ping_time    = Unix.gettimeofday () +. ping_interval;
    pings_outstanding = 0;
    io_thread         = Thread.self ();
    msg_thread        = Thread.self ();
    msg_queue         = SyncQueue.create ();
    err               = None;
  } in

  begin
    try
      process_expected_info conn ?timeout:(remaining_time ());
      send_connect conn ?timeout:(remaining_time ());

      ignore @@ subscribe conn (conn.resp_sub_prefix ^ "*")
        ~callback:begin fun msg ->
          let req = Mutex.protect conn.mutex (fun () -> conn.cur_request) in
          match req with
          | Some req when msg.subject = req.inbox ->
            PendingRequest.(resolve req (Response msg))
          | _ -> ()
        end
    with exn ->
      begin
        try
          shutdown conn.sock SHUTDOWN_ALL
        with Unix_error (ENOTCONN, _, _) ->
          ()
      end;
      Unix.close conn.sock;
      Faraday.close conn.serializer;
      raise exn
  end;

  conn.io_thread  <- Thread.create io_loop conn;
  conn.msg_thread <- Thread.create msg_loop conn;

  conn

let with_client
    ?url
    ?name
    ?verbose
    ?pedantic
    ?connect_timeout
    ?ping_interval
    ?max_pings_outstanding
    ?closed_cb
    ?error_cb
    ?inbox_prefix
    f =
  let c = connect
      ?url
      ?name
      ?verbose
      ?pedantic
      ?connect_timeout
      ?ping_interval
      ?max_pings_outstanding
      ?closed_cb
      ?error_cb
      ?inbox_prefix
      ()
  in
  Fun.protect
    ~finally:(fun () -> if state c = Connected then drain c)
    (fun () -> f c)
