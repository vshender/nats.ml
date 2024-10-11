(** A module representing NATS subscription. *)

open Compat

type callback = Message.t -> unit

(** A module for managing a thread-safe message queue. *)
module MessageQueue = struct
  (** A type representing a thread-safe message queue. *)
  type t = {
    messages : Message.t Queue.t;
    (** A queue of pending messages. *)
    mutex : Mutex.t;
    (** A mutex for synchronizing access to the message queue. *)
    condition : Condition.t;
    (** A condition variable for signaling a new message or timeout. *)
  }

  (** [create ()] creates an empty message queue. *)
  let create () = {
    messages  = Queue.create ();
    mutex     = Mutex.create ();
    condition = Condition.create ();
  }

  (** [length t] returns the number of messages currently in the queue [t]. *)
  let length t =
    Mutex.protect t.mutex (fun () -> Queue.length t.messages)

  (** [is_empty t] checks if the message queue [t] is empty. *)
  let is_empty t =
    length t = 0

  (** [get ?timeout_time t] retrieves the next message from the message queue
      [t].  If the queue is empty, it blocks until a message is available or
      the timeout occurs. *)
  let get ?timeout_time t =
    let timed_out () =
      match timeout_time with
      | Some timeout_time -> Unix.gettimeofday () > timeout_time
      | None              -> false
    in
    Mutex.protect t.mutex
      begin fun () ->
        while Queue.is_empty t.messages && not (timed_out ()) do
          Condition.wait t.condition t.mutex
        done;
        Queue.take_opt t.messages
      end

  (** [put t msg] adds a new message [msg] to the message queue [t].  If the
      queue was previously empty, it notifies any waiting threads that a new
      message is available. *)
  let put t msg =
    Mutex.protect t.mutex
      begin fun () ->
        Queue.add msg t.messages;
        if Queue.length t.messages = 1 then
          Condition.signal t.condition
      end

  (** [signal_timeout t] signals the blocked call of [next_msg] to check if a
      timeout has occured.  This can be used to wake up any waiting threads so
      they can re-evaluate the timeout condition. *)
  let signal_timeout t =
    Mutex.protect t.mutex
      (fun () -> Condition.signal t.condition)
end

(** A type representing the delivery mechanism for messages. *)
type message_delivery =
  | Callback of callback        (** Asynchronous delivery via a callback. *)
  | Queue    of MessageQueue.t  (** Synchronous delivery via a message queue. *)

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
    | None    -> Queue (MessageQueue.create ())
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
  | Queue q -> MessageQueue.signal_timeout q

let handle_msg t msg =
  begin match t.delivery with
    | Callback cb -> cb msg
    | Queue q     -> MessageQueue.put q msg
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
    if is_closed t && MessageQueue.is_empty q then
      failwith "subscription is closed";

    let timeout_time =
      timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

    t.next_msg_start_cb |> Option.iter (fun cb -> cb t timeout_time);
    Fun.protect
      ~finally:(fun () -> t.next_msg_finish_cb |> Option.iter (fun cb -> cb t))
      (fun () -> MessageQueue.get ?timeout_time q)
