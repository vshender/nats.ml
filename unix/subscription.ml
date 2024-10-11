(** A module representing NATS subscription. *)

open Compat

type callback = Message.t -> unit

type t = {
  sid : int;
  (** The subscription identifier. *)
  subject : string;
  (** The subject to which the subscription is bound. *)
  group : string option;
  (** An optional group name for queue subscriptions. *)

  queue : Message.t MessageQueue.t;
  (** A pending messages queue. *)
  callback : callback option;
  (** Message handling function, if any. *)

  mutable closed : bool;
  (** Is subscription closed? *)
  mutable delivered : int;
  (** The number of messages delivered so far. *)
  mutable max_msgs : int option;
  (** An optional limit on the number of messages to deliver before
      unsubscribing. *)
  mutex : Mutex.t;
  (** A mutex for thread-safe access to subscription state. *)

  unsubscribe_cb : (t -> unit) option;
  (** An optional callback invoked when unsubscribing. *)
  remove_subscription_cb : (t -> unit) option;
  (** An optional callback invoked when the subscription is removed. *)
  next_msg_start_cb : (t -> float option -> unit) option;
  (** An optional callback invoked before retrieving the next message. *)
  next_msg_finish_cb : (t -> unit) option;
  (** An optional callback invoked after retrieving the next message. *)
}

let sid t = t.sid

let subject t = t.subject

let group t = t.group

let is_sync t =
  Option.is_none t.callback

let is_closed t =
  Mutex.protect t.mutex (fun () -> t.closed)

let delivered t =
  Mutex.protect t.mutex (fun () -> t.delivered)

let max_msgs t =
  Mutex.protect t.mutex (fun () -> t.max_msgs)

let pending_msgs t =
  MessageQueue.length t.queue

let create
    ?unsubscribe_cb ?remove_subscription_cb
    ?next_msg_start_cb ?next_msg_finish_cb
    sid subject group callback =
  {
    sid;
    subject;
    group;
    queue = MessageQueue.create ();
    callback;
    closed    = false;
    delivered = 0;
    max_msgs  = None;
    mutex     = Mutex.create ();
    unsubscribe_cb;
    remove_subscription_cb;
    next_msg_start_cb;
    next_msg_finish_cb;
  }

let close t =
  Mutex.protect t.mutex (fun () -> t.closed <- true)

let unsubscribe ?max_msgs t =
  let remove_sub =
    Mutex.protect t.mutex
      begin fun () ->
        t.max_msgs <- max_msgs;
        Option.(is_none max_msgs || t.delivered >= get max_msgs)
      end
  in
  t.unsubscribe_cb |> Option.iter (fun cb -> cb t);
  if remove_sub then begin
    t.remove_subscription_cb |> Option.iter (fun cb -> cb t);
    close t
  end

let signal_timeout t =
  if not (is_sync t) then
    failwith "signal_timeout should only be called for synchronous subscription";
  MessageQueue.signal_timeout t.queue

let handle_msg t msg =
  begin match t.callback with
    | Some callback -> callback msg
    | None          -> MessageQueue.put t.queue msg
  end;

  let remove_sub =
    Mutex.protect t.mutex
      begin fun () ->
        t.delivered <- t.delivered + 1;
        Option.(is_some t.max_msgs && t.delivered >= get t.max_msgs)
      end
  in
  if remove_sub then begin
    t.remove_subscription_cb |> Option.iter (fun cb -> cb t);
    close t
  end

let next_msg ?timeout t =
  if not (is_sync t) then
    failwith "get_message should only be called for synchronous subscription";

  if is_closed t && MessageQueue.is_empty t.queue then
    failwith "subscription is closed";

  let timeout_time =
    timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

  t.next_msg_start_cb |> Option.iter (fun cb -> cb t timeout_time);
  Fun.protect
    ~finally:(fun () -> t.next_msg_finish_cb |> Option.iter (fun cb -> cb t))
    (fun () -> MessageQueue.get ?timeout_time t.queue)
