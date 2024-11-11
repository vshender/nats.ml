(** The module for managing a fake NATS server for edge case testing
    purposes. *)

(** The type representing a fake NATS server instance. *)
type t

(** [run ?port msgs] starts a fake NATS server on the given [port] and
    processes a list of messages [msgs].

    Each message can be:

    - [`Msg msg]: send the string [msg] to the connected client.
    - [`Read]: read data from the connected client.
    - [`Delay n]: delay for [n] seconds.
*)
val run : ?port:int -> [< `Msg of string | `Read | `Delay of float ] list -> t

(** [client_url t] is the URL for a client to connect to the fake NATS
    server. *)
val client_url : t -> string
