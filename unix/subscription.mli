(** The module for managing NATS subscriptions.

    This module providing functionality for both synchronous and asynchronous
    message handling in a thread-safe manner.
*)

(** The type of NATS subscriptions. *)
type t

(** The type of callback functions for message handling. *)
type callback = Message.t -> unit

(** [create ?schedule_message_handling ?sync_op_started ?sync_op_finished ~flush ~unsubscribe ~remove_subscription sid subject group callback]
    creates a new NATS subscription with the given parameters.

    - [sid]: the unique subscription identifier.
    - [subject]: the subject to which the subscription is subscribed.
    - [group]: an optional group name for queue subscriptions.
    - [callback]: an optional callback for handling messages asynchronously.
    - [flush]: a function to flush pending operations.
    - [unsubscribe]: a function to perform unsubscription.
    - [remove_subscription]: a function to deregister the subscription.
    - [schedule_message_handling]: an optional function to schedule
      asynchronous message handling.  If not provided, a message will be
      handled synchronously in the same thread that calls the {!handle_msg}
      function.
    - [sync_op_started]: an optional callback invoked when a synchronous
      operation starts.
    - [sync_op_finished]: an optional callback invoked when a synchronous
      operation finishes.

    Raises [NatsError AsyncSubRequired] if [schedule_message_handling] is
    provided for a synchronous subscription. *)
val create :
  ?schedule_message_handling : (t -> msg:Message.t -> unit) ->
  ?sync_op_started :
    (t ->
     signal_interrupt:(Errors.nats_error -> unit) ->
     timeout_time:float option ->
     unit) ->
  ?sync_op_finished : (t -> unit) ->
  flush : (t -> timeout:float option -> unit) ->
  unsubscribe : (t -> unit) ->
  remove_subscription : (t -> unit) ->
  int -> string -> string option -> callback option -> t

(** [sid t] returns the subscription identifier (sid) of [t]. *)
val sid : t -> int

(** [subject t] returns the subject to which the subscription [t] is
    subscribed. *)
val subject : t -> string

(** [group t] returns the optional group name of the subscription [t], if the
    subscription is part of a group. *)
val group : t -> string option

(** [callback t] returns the callback for handling incoming messages. *)
val callback : t -> callback option

(** [is_sync t] returns [true] if the subscription [t] is synchronous, and
    [false] otherwise. *)
val is_sync : t -> bool

(** [is_closed t] returns [true] if the subscription [t] is closed, and [false]
    otherwise. *)
val is_closed : t -> bool

(** [delivered t] returns the number of messages that have been delivered to
    the subscription [t]. *)
val delivered : t -> int

(** [max_msgs t] returns the optional maximum number of messages the
    subscription [t] will receive.

    If set, the subscription automatically unsubscribes after receiving the
    specified number of messages. *)
val max_msgs : t -> int option

(** [pending_msgs t] returns the number of pending messages in the internal
    message queue of the subscription [t]. *)
val pending_msgs : t -> int

(** [close t] closes the subscription [t], preventing it from receiving further
    messages. *)
val close : t -> unit

(** [handle_msg t msg] handles an incoming message [msg] for the subscription
    [t].  This function adds the message to the internal queue, and if an
    asynchronous callback is provided, schedules its handling based on the
    provided scheduling function.

    Raises [NatsError SubscriptionClosed] if the subscription [t] is closed. *)
val handle_msg : t -> Message.t -> unit

(** [next_msg_internal t] attempts to retrieve the next message from the
    subscription's internal queue.  If the queue is empty, it returns [None]
    immediately.

    This function is intended for internal use by the library. *)
val next_msg_internal : t -> Message.t option

(** [next_msg ?timeout t] retrieves the next message for the synchronous
    subscription [t], with an optional timeout (in seconds).  If there are no
    pending messages in the subscription's internal message queue, the function
    blocks until a message arrives or the timeout expires (if specified).
    Returns [None] if no message is available after the timeout.

    Raises:

    - [NatsError SyncSubRequired] if called on an asynchronous subscription.
    - [NatsError SubscriptionClosed] if the subscription is closed and there
      are no pending messages.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
*)
val next_msg : ?timeout:float -> t -> Message.t option

(** [unsubscribe ?max_msgs t] unsubscribes the subscription [t].

    If [max_msgs] is specified, the subscription will unsubscribe automatically
    after receiving the specified number of messages.

    Raises [NatsError SubscriptionClosed] if the subscription [t] is already
    closed. *)
val unsubscribe : ?max_msgs:int -> t -> unit

(** [drain ?timeout t] unsubscribes the subscription [t] and waits for all
    pending messages to be processed, with an optional timeout (in seconds).

    If [timeout] is specified, the function will raise an exception if all
    pending messages are not processed within the specified time limit.  In
    this case the function also clears the internal message queue.

    Raises:

    - [NatsError AsyncSubRequired] if the subscription [t] is synchronous.
    - [NatsError SubscriptionClosed] if the subscription [t] is already closed.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
    - [NatsError Timeout] if the drain operation times out.
*)
val drain : ?timeout:float -> t -> unit
