(** The module for managing NATS subscriptions. *)

open Compat

open Nats

type callback = Message.t -> unit

(** The type of NATS subscriptions. *)
type t = {
  sid : int;
  (** The unique subscription identifier. *)
  subject : string;
  (** The subject to which the subscription is subscribed. *)
  group : string option;
  (** An optional group name for queue subscriptions. *)
  callback : callback option;
  (** An optional callback for handling messages asynchronously. *)

  queue : Message.t SyncQueue.t;
  (** The pending message queue. *)

  mutable closed : bool;
  (** Indicates whether the subscription is closed? *)
  mutable delivered : int;
  (** The number of messages delivered to the subscription. *)
  mutable max_msgs : int option;
  (** An optional limit on the number of messages to deliver before
      unsubscribing. *)
  mutex : Mutex.t;
  (** A mutex for thread-safe access to the subscription state. *)

  flush : t -> timeout:float option -> unit;
  (** A function to flush pending operations. *)
  unsubscribe : t -> unit;
  (** A function to perform unsubscription. *)
  remove_subscription : t -> unit;
  (** A function to deregister the subscription. *)
  schedule_message_handling : (t -> msg:Message.t -> unit) option;
  (** An optional function to schedule asynchronous message handling. *)
  sync_op_started : (t ->
                     signal_interrupt:(Errors.t -> unit) ->
                     timeout_time:float option ->
                     unit) option;
  (** An optional callback invoked when a synchronous operation starts. *)
  sync_op_finished : (t -> unit) option;
  (** An optional callback invoked when a synchronous operation finishes. *)
}

let create
    ?schedule_message_handling
    ?sync_op_started ?sync_op_finished
    ~flush ~unsubscribe ~remove_subscription
    sid subject group callback =
  if Option.is_none callback && Option.is_some schedule_message_handling then
    Errors.nats_error AsyncSubRequired;
  {
    sid;
    subject;
    group;
    queue     = SyncQueue.create ();
    callback;
    closed    = false;
    delivered = 0;
    max_msgs  = None;
    mutex     = Mutex.create ();
    schedule_message_handling;
    flush;
    unsubscribe;
    remove_subscription;
    sync_op_started;
    sync_op_finished;
  }

let sid t = t.sid

let subject t = t.subject

let group t = t.group

let callback t = t.callback

let queue t = t.queue

let is_sync t =
  Option.is_none t.callback

let is_closed t =
  Mutex.protect t.mutex (fun () -> t.closed)

let delivered t =
  Mutex.protect t.mutex (fun () -> t.delivered)

let pending_msgs t =
  SyncQueue.length t.queue

let max_msgs t =
  Mutex.protect t.mutex (fun () -> t.max_msgs)

let close t =
  Mutex.protect t.mutex (fun () -> t.closed <- true)

let handle_msg t msg =
  if is_closed t then
    Errors.nats_error SubscriptionClosed;

  SyncQueue.put t.queue msg;

  t.callback |> Option.iter
    begin fun callback ->
      match t.schedule_message_handling with
      | Some schedule -> schedule t ~msg
      | None          -> callback @@ Option.get @@ SyncQueue.get t.queue
    end;

  let remove_sub =
    Mutex.protect t.mutex
      begin fun () ->
        t.delivered <- t.delivered + 1;
        Option.(is_some t.max_msgs && t.delivered >= get t.max_msgs)
      end
  in
  if remove_sub then begin
    t.remove_subscription t;
    close t
  end

(** [with_sync_op ?timeout t f] wraps a synchronous operation on the
    subscription [t], optionally specifying a timeout. *)
let with_sync_op ?timeout t f =
  let sync_op_err = ref None in
  let interrupt_cond () = Option.is_some !sync_op_err
  and signal_interrupt err =
    sync_op_err := Some err;
    SyncQueue.signal_interrupt t.queue
  and timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout)
  in
  t.sync_op_started |> Option.iter (fun cb -> cb t ~signal_interrupt ~timeout_time);
  Fun.protect
    ~finally:(fun () -> t.sync_op_finished |> Option.iter (fun cb -> cb t))
    begin fun () ->
      let result = f interrupt_cond in
      match !sync_op_err with
      | Some err -> Errors.nats_error err
      | None     -> result
    end

let next_msg ?timeout t =
  if not (is_sync t) then
    Errors.nats_error SyncSubRequired;
  if is_closed t && SyncQueue.is_empty t.queue then
    Errors.nats_error SubscriptionClosed;

  Option.get @@
  with_sync_op ?timeout t
    (fun interrupt_cond -> SyncQueue.get ~interrupt_cond t.queue)

let next_msg_opt ?timeout t =
  try
    Some (next_msg t ?timeout)
  with Errors.NatsError Timeout ->
    None

let unsubscribe ?max_msgs t =
  if is_closed t then
    Errors.nats_error SubscriptionClosed;

  let remove_sub =
    Mutex.protect t.mutex
      begin fun () ->
        t.max_msgs <- max_msgs;
        Option.(is_none max_msgs || t.delivered >= get max_msgs)
      end
  in
  t.unsubscribe t;
  if remove_sub then begin
    t.remove_subscription t;
    close t
  end

let drain ?timeout t =
  if is_sync t then
    Errors.nats_error AsyncSubRequired;
  if is_closed t then
    Errors.nats_error SubscriptionClosed;

  let timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

  Fun.protect
    ~finally:begin fun () ->
      t.remove_subscription t;
      close t
    end
    begin fun () ->
      t.unsubscribe t;
      t.flush t ~timeout;

      (* Calculate remaining execution time. *)
      let timeout =
        timeout_time
        |> Option.map (fun timeout_time -> timeout_time -. Unix.gettimeofday ())
      in

      (* Wait until all pending messages are processed. *)
      with_sync_op ?timeout t
        begin fun interrupt_cond ->
          if not (SyncQueue.join ~interrupt_cond t.queue) then
            SyncQueue.clear t.queue;
        end
    end
