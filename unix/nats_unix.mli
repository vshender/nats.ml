(** A [Unix] library based client for NATS. *)

module Client = Client

(** A module representing NATS subscription. *)
module Subscription : sig
  (** The type of NATS subscriptions. *)
  type t = Subscription.t

  (** [sid t] returns the subscription ID (sid) of the subscription [t]. *)
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

  (** [is_closed t] returns [true] if the subscription [t] is closed, and [false]
      otherwise. *)
  val is_closed : t -> bool

  (** [delivered t] returns the number of messages that have been delivered to
      the subscription [t]. *)
  val delivered : t -> int

  (** [unsubscribe ?max_msgs t] unsubscribes from the subscription [t].

      If [max_msgs] is specified, the subscription will unsubscribe automatically
      after receiving the specified number of messages. *)
  val unsubscribe : ?max_msgs:int -> t -> unit

  (** [next_msg ?timeout t] retrieves the next message for the synchronous
      subscription [t], with an optional timeout.  Fails if the subscription is
      asynchrnonous. *)
  val next_msg : ?timeout:float -> t -> Message.t option
end

(* A module containing a type of NATS message headers and functions for working
   with it. *)
module Headers = Message.Headers

module Message = Message
