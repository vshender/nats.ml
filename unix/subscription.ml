(** A module for managing NATS subscriptions. *)

open Compat

type callback = Message.t -> unit

(** The type of NATS subscriptions. *)
type t = {
  sid : int;
  (** The subscription identifier. *)
  subject : string;
  (** The subject to which the subscription is subscribed. *)
  group : string option;
  (** An optional group name for queue subscriptions. *)
  callback : callback option;
  (** An optional callback for handling messages asynchronously. *)

  queue : Message.t SyncQueue.t;
  (** A pending messages queue. *)

  mutable closed : bool;
  (** Indicates whether the subscription is closed? *)
  mutable delivered : int;
  (** The number of messages delivered to the subscription. *)
  mutable max_msgs : int option;
  (** An optional limit on the number of messages to deliver before
      unsubscribing. *)
  mutex : Mutex.t;
  (** A mutex for thread-safe access to the subscription state. *)

  unsubscribe : t -> unit;
  (** A function to perform unsubscription. *)
  remove_subscription : t -> unit;
  (** A function to deregister the subscription. *)
  schedule_message_handling : (t -> msg:Message.t -> unit) option;
  (** An optional function to schedule asynchronous message handling. *)
  sync_op_started : (t -> timeout_time:float option -> unit) option;
  (** An optional callback invoked when a synchronous operation starts. *)
  sync_op_finished : (t -> unit) option;
  (** An optional callback invoked when a synchronous operation finishes. *)
}

let create
    ?schedule_message_handling
    ?sync_op_started ?sync_op_finished
    ~unsubscribe ~remove_subscription
    sid subject group callback =
  if Option.is_none callback && Option.is_some schedule_message_handling then
    failwith "cannot use schedule_message_handling with synchronous subscriptions";
  {
    sid;
    subject;
    group;
    queue = SyncQueue.create ();
    callback;
    closed    = false;
    delivered = 0;
    max_msgs  = None;
    mutex     = Mutex.create ();
    schedule_message_handling;
    unsubscribe;
    remove_subscription;
    sync_op_started;
    sync_op_finished;
  }

let sid t = t.sid

let subject t = t.subject

let group t = t.group

let callback t = t.callback

let is_sync t =
  Option.is_none t.callback

let is_closed t =
  Mutex.protect t.mutex (fun () -> t.closed)

let delivered t =
  Mutex.protect t.mutex (fun () -> t.delivered)

let max_msgs t =
  Mutex.protect t.mutex (fun () -> t.max_msgs)

let pending_msgs t =
  SyncQueue.length t.queue

let close t =
  Mutex.protect t.mutex (fun () -> t.closed <- true)

let unsubscribe ?max_msgs t =
  if is_closed t then
    failwith "subscription is closed";

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

let handle_msg t msg =
  if is_closed t then
    failwith "subscription is closed";

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

let next_msg_internal t =
  Option.get @@ SyncQueue.get t.queue

(** [with_sync_op ?timeout t f] wraps a synchronous operation on the
    subscription [t], optionally specifying a timeout. *)
let with_sync_op ?timeout t f =
  let timeout_time, timed_out =
    match timeout with
    | Some timeout ->
      let timeout_time = Unix.gettimeofday () +. timeout in
      let timed_out () = Unix.gettimeofday () > timeout_time in
      Some timeout_time, Some timed_out
    | None ->
      None, None
  in
  t.sync_op_started |> Option.iter (fun cb -> cb t ~timeout_time);
  Fun.protect
    ~finally:(fun () -> t.sync_op_finished |> Option.iter (fun cb -> cb t))
    (fun () -> f timed_out)

let next_msg ?timeout t =
  if not (is_sync t) then
    failwith "next_msg is only valid for synchronous subscriptions";
  if is_closed t && SyncQueue.is_empty t.queue then
    failwith "subscription is closed";

  with_sync_op ?timeout t
    (fun interrupt_cond -> SyncQueue.get ?interrupt_cond t.queue)

let signal_timeout t =
  if not (is_sync t) then
    failwith "signal_timeout should only be called for synchronous subscription";
  SyncQueue.signal_interrupt t.queue
