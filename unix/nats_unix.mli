(** A [Unix] library based client for NATS. *)

(** The type of NATS errors. *)
type nats_error = Errors.nats_error

(** [NatsError] is a NATS error exception. *)
exception NatsError of nats_error

module Client = Client

(** The module for managing NATS subscriptions. *)
module Subscription : sig
  (** The type of NATS subscriptions. *)
  type t = Subscription.t

  (** [sid t] returns the subscription identifier (sid) of [t]. *)
  val sid : t -> int

  (** [subject t] returns the subject to which the subscription [t] is
      subscribed. *)
  val subject : t -> string

  (** [group t] returns the optional group name of the subscription [t], if the
      subscription is part of a group. *)
  val group : t -> string option

  (** [is_sync t] returns [true] if the subscription [t] is synchronous, and
      [false] otherwise. *)
  val is_sync : t -> bool

  (** [is_closed t] returns [true] if the subscription [t] is closed, and
      [false] otherwise. *)
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

  (** [next_msg ?timeout t] retrieves the next message for the synchronous
      subscription [t], with an optional timeout (in seconds).  If there are no
      pending messages in the subscription's internal message queue, the
      function blocks until a message arrives or the timeout expires (if
      specified).  Returns [None] if no message is available after the timeout.

    Raises:

    - [NatsError SyncSubRequired] if called on an asynchronous subscription.
    - [NatsError SubscriptionClosed] if the subscription is closed and there
      are no pending messages.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
  *)
  val next_msg : ?timeout:float -> t -> Message.t option

  (** [unsubscribe ?max_msgs t] unsubscribes the subscription [t].e

      If [max_msgs] is specified, the subscription will unsubscribe
      automatically after receiving the specified number of messages.

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
      - [NatsError SubscriptionClosed] if the subscription [t] is already
        closed.
      - [NatsError ConnectionLost] if the connection is lost during the
        operation.
      - [NatsError Timeout] if the drain operation times out.
  *)
  val drain : ?timeout:float -> t -> unit
end

(** The NATS message headers module. *)
module Headers = Message.Headers

module Message = Message
