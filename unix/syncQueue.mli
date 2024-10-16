(** A module for managing thread-safe queues.

    The [SyncQueue] module provides a thread-safe queue that ensures safe
    access and modification across multiple threads.  It supports a variety of
    queue operations, offering both blocking and non-blocking options.
    Additionally, it allows blocking operations to be interrupted using custom
    conditions, giving you precise control over multi-threaded processing.
*)

(** A type representing a thread-safe message queue. *)
type 'a t

(** [create ()] creates an empty message queue. *)
val create : unit -> 'a t

(** [length t] returns the number of messages currently in the queue [t]. *)
val length : 'a t -> int

(** [is_empty t] checks if the message queue [t] is empty. *)
val is_empty : 'a t -> bool

(** [try_get t] attempts to retrieve the next message from the message queue
    [t] without blocking.  If the queue is empty, it returns [None]
    immediately.

    If a message is successfully retrieved and the queue becomes empty
    afterward, the function notifies any waiting threads about it. *)
val try_get : 'a t -> 'a option

(** [get ?interrupt_cond t] retrieves the next message from the message queue
    [t].  If the queue is empty, the function blocks until a message is
    available or the interrupt condition is met.

    If a message is successfully retrieved and the queue becomes empty
    afterward, the function notifies any waiting threads about it. *)
val get : ?interrupt_cond:(unit -> bool) -> 'a t -> 'a option

(** [clear t] discards all elements from the message queue [t].

    If the message queue was nonempty before clearing, the function notifies
    any waiting threads that the message queue is now empty. *)
val clear : 'a t -> unit

(** [join ?interrupt_cond t] waits until the message queue [t] becomes empty.
    If the queue is nonempty, the function blocks until the queue is empty or
    the interrupt condition is met.

    Returns [true] if the queue is empty when the function exits, or [false] if
    the interrupt condition breaks the wait and the queue still contains
    messages. *)
val join : ?interrupt_cond:(unit -> bool) -> 'a t -> bool

(** [put t msg] adds a new message [msg] to the message queue [t].  If the
    queue was previously empty, it notifies any waiting threads that a new
    message is available. *)
val put : 'a t -> 'a -> unit

(** [signal_interrupt t] wakes up any threads that are waiting on the message
    queue [t] to recheck their interrupt conditions.  This can be used to
    interrupt blocking operations such as {!get} or {!join}. *)
val signal_interrupt : 'a t -> unit
