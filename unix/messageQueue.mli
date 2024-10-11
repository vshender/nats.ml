(** A module for managing a thread-safe message queue. *)

(** A type representing a thread-safe message queue. *)
type 'a t

(** [create ()] creates an empty message queue. *)
val create : unit -> 'a t

(** [length t] returns the number of messages currently in the queue [t]. *)
val length : 'a t -> int

(** [is_empty t] checks if the message queue [t] is empty. *)
val is_empty : 'a t -> bool

(** [try_get t] attempts to retrieve the next message from the message queue
    [t] without blocking.  If the queue becomes empty, the function notifies
    any waiting threads about it. *)
val try_get : 'a t -> 'a option

(** [get ?timeout_time t] retrieves the next message from the message queue
    [t].  If the queue is empty, it blocks until a message is available or the
    timeout occurs (the current time reaches [timeout_time]).  If the queue
    becomes empty, the function notifies any waiting threads about it. *)
val get : ?timeout_time:float -> 'a t -> 'a option

(** [join ?timeout_time t] waits until the message queue [t] becomes empty.  If
    the queue is nonempty, it blocks until the queue is empty or the timeout
    occurs (the current time reaches [timeout_time]).

    Returns [true] if the queue is empty when the function exits, or [false] if
    the timeout occurred before the queue became empty. *)
val join : ?timeout_time:float -> 'a t -> bool

(** [put t msg] adds a new message [msg] to the message queue [t].  If the
    queue was previously empty, it notifies any waiting threads that a new
    message is available. *)
val put : 'a t -> 'a -> unit

(** [signal_timeout t] signals the blocked call of [next_msg] to check if a
    timeout has occured.  This can be used to wake up any waiting threads so
    they can re-evaluate the timeout condition. *)
val signal_timeout : 'a t -> unit
