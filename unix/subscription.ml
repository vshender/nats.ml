(** A module representing NATS subscription. *)

open Compat

type callback = Message.t -> unit

(** A type representing a message queue for synchronous message delivery. *)
type message_queue = {
  messages : Message.t Queue.t;
  (** A queue of pending messages. *)
  mutex : Mutex.t;
  (** A mutex for synchronizing access to the message queue. *)
  condition : Condition.t;
  (** A condition variable for signaling a new message or timeout. *)
}

(** A type representing the delivery mechanism for messages. *)
type message_delivery =
  | Callback of callback       (** Asynchronous delivery via a callback. *)
  | Queue    of message_queue  (** Synchronous delivery via a message queue. *)

type t = {
  sid : int;
  (** The subscription identifier. *)
  subject : string;
  (** The subject to which the subscription is bound. *)
  group : string option;
  (** An optional group name for queue subscriptions. *)
  delivery : message_delivery;
  (** The message delivery mechanism. *)

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
  match t.delivery with
  | Callback _ -> false
  | Queue _    -> true

let is_closed t =
  Mutex.protect t.mutex (fun () -> t.closed)

let delivered t =
  Mutex.protect t.mutex (fun () -> t.delivered)

let max_msgs t =
  Mutex.protect t.mutex (fun () -> t.max_msgs)

let create
    ?unsubscribe_cb ?remove_subscription_cb
    ?next_msg_start_cb ?next_msg_finish_cb
    sid subject group callback =
  let delivery = match callback with
    | Some cb -> Callback cb
    | None    -> Queue {
        messages  = Queue.create ();
        mutex     = Mutex.create ();
        condition = Condition.create ();
      }
  in {
    sid;
    subject;
    group;
    delivery;
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
  match t.delivery with
  | Callback _ ->
    failwith "signal_timeout should only be called for synchronous subscription"
  | Queue q ->
    Mutex.protect q.mutex
      (fun () -> Condition.signal q.condition)

let handle_msg t msg =
  begin match t.delivery with
    | Callback cb -> cb msg
    | Queue q     ->
      Mutex.protect q.mutex
        begin fun () ->
          Queue.add msg q.messages;
          if Queue.length q.messages = 1 then
            Condition.signal q.condition
        end
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
  match t.delivery with
  | Callback _ ->
    failwith "get_message should only be called for synchronous subscription"
  | Queue q ->
    if is_closed t
    && Mutex.protect q.mutex (fun () -> Queue.is_empty q.messages) then
      failwith "subscription is closed";

    let timeout_time =
      timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in
    let timed_out () =
      match timeout_time with
      | Some timeout_time -> Unix.gettimeofday () > timeout_time
      | None              -> false
    in

    t.next_msg_start_cb |> Option.iter (fun cb -> cb t timeout_time);
    Fun.protect
      ~finally:(fun () -> t.next_msg_finish_cb |> Option.iter (fun cb -> cb t))
      begin fun () ->
        Mutex.protect q.mutex
          begin fun () ->
            while Queue.is_empty q.messages && not (timed_out ()) do
              Condition.wait q.condition q.mutex
            done;
            Queue.take_opt q.messages
          end
      end
