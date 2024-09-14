(** Utility functions for working with sockets in OCaml, allowing connection
    and reading operations with support for specifying timeouts.
*)

open Unix

(** Exception raised when a timeout occurs during a socket operation. *)
exception Timed_out

(** [connect ?timeout sock addr] attempts to connect the socket [sock] to the
    address [addr] with an optional [timeout] (in seconds).

    If the [timeout] is exceeded during the connection attempt, the [Timed_out]
    exception is raised.  If the connection fails, an appropriate [Unix_error]
    is raised.

    By default (or if [timeout] is negative), the function does not time out,
    meaning it will wait indefinitely for the connection to succeed. *)
val connect : ?timeout:float -> file_descr -> sockaddr -> unit

(** [read ?timeout fd buf pos len] reads data from the descriptor [fd] into the
    buffer [buf] starting at position [pos] and reading up to [len] bytes.
    Returns the number of bytes actually read.

    This function waits until the descriptor is ready for reading, with an
    optional [timeout] specified in seconds.  If the descriptor is not readable
    within the given timeout, the [Timed_out] exception is raised.

    By default (or if [timeout] is negative), the function does not time out,
    meaning it will wait indefinitely for the descriptor to become readable. *)
val read : ?timeout:float -> file_descr -> bytes -> int -> int -> int
