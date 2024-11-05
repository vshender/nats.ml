(** The module for managing a local instance of the NATS server for testing
    purposes.

    It is useful for managing a server instance during integration tests,
    ensuring that a clean environment is set up before each test run.
*)

(** The type representing a NATS server instance. *)
type t

(** [create ?id ?port ()] creates a new server instance with an optional [id]
    and [port].  If [port] is not specified, a random available port will be
    chosen. *)
val create : ?id:string -> ?port:int -> unit -> t

(** [start t] starts the NATS server instance [t].

    Raises [Failure] if the server is already running. *)
val start : t -> unit

(** [stop t] stops the NATS server instance [t].

    Raises [Failure] if the server is not running. *)
val stop : t -> unit

(** [with_server ?id ?port f] starts a NATS server, executes the function [f],
    and then stops the server. *)
val with_server : ?id:string -> ?port:int -> (t -> 'a) -> 'a

(** [is_running t] returns [true] if the NATS server instance [t] is currently
    running, and [false] otherwise. *)
val is_running : t -> bool

(** [client_url t] returns the URL that clients can use to connect to the NATS
    server.

    Raises [Failure] if the server is not running. *)
val client_url : t -> string
