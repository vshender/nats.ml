(** A module representing NATS subscription. *)

(** The type of NATS subscriptions. *)
type t

(** The type of callback functions invoked when a message is received. *)
type callback = Message.t -> unit

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

(** [max_msgs t] returns the optional maximum number of messages the
    subscription [t] will receive.

    If set, the subscription automatically unsubscribes after receiving the
    specified number of messages. *)
val max_msgs : t -> int option

(** [pending_msgs t] returns the number of pending messages in the internal
    message queue of the subscription [t]. *)
val pending_msgs : t -> int

(** [create ?unsubscribe_cb ?remove_subscription_cb ?next_msg_start_cb ?next_msg_finish_cb sid subject group callback]
    creates a new subscription with the specified parameters.

    - [unsubscribe_cb] (optional): a callback function invoked when the
      subscription is unsubscribed.
    - [remove_subscription_cb] (optional): a callback invoked when the
      subscription can be removed.
    - [next_msg_start_cb] (optional): callback invoked when a [next_msg]
      request starts, taking the optional timeout.
    - [next_msg_finish_cb] (optional): callback invoked when a [next_msg]
      request finishes.
    - [sid]: the subscription ID.
    - [subject]: the subject to which the subscription is created.
    - [group] (optional): the group name for queue subscriptions.
    - [callback] (optional): the function to handle incoming messages.
*)
val create :
  ?unsubscribe_cb : (t -> unit) ->
  ?remove_subscription_cb : (t -> unit) ->
  ?next_msg_start_cb : (t -> float option -> unit) ->
  ?next_msg_finish_cb : (t -> unit) ->
  int -> string -> string option -> callback option -> t

(** [close t] closes the subscription [t], preventing it from receiving further
    messages. *)
val close : t -> unit

(** [unsubscribe ?max_msgs t] unsubscribes from the subscription [t].

    If [max_msgs] is specified, the subscription will unsubscribe automatically
    after receiving the specified number of messages. *)
val unsubscribe : ?max_msgs:int -> t -> unit

(** [signal_timeout t] signals a timeout for the currently invoking [next_msg]
    call on subscription [t]. *)
val signal_timeout : t -> unit

(** [handle_msg t msg] processes a received message [msg] for the subscription
    [t]. *)
val handle_msg : t -> Message.t -> unit

(** [next_msg ?timeout t] retrieves the next message for the synchronous
    subscription [t], with an optional timeout.  Fails if the subscription is
    asynchrnonous. *)
val next_msg : ?timeout:float -> t -> Message.t option
