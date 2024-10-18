(** Parsing of NATS client protocol messages produced by a server.

    See {{:https://docs.nats.io/reference/reference-protocols/nats-protocol} Client Protocol}
    for details.
*)

(** [server_message] parses a NATS client protocol message produced by a
    server. *)
val server_message : Protocol.ServerMessage.t Angstrom.t

(** The reader for NATS client protocol messages produced by a server. *)
module Reader : sig
  (** The type representing a NATS messages reader. *)
  type t

  (** The type representing the result of parsing a NATS message. *)
  type result =
    | Message of Protocol.ServerMessage.t
    (** A successfully parsed NATS message. *)
    | Need_input
    (** More input is needed to complete the parsing. *)
    | Parse_error of string list * string
    (** A parsing error, with a list of parsing markers and an error message. *)

  (** [create ?initial_buffer_size ()] creates a new NATS message reader. *)
  val create : ?initial_buffer_size:int -> unit -> t

  (** [next_msg reader] examines the current state of [reader] and returns the
      result of the parsing.

      If a message was successfully parsed, the subsequent call to [next_msg]
      will attempt to parse the next message. *)
  val next_msg : t -> result

  (** [feed reader bytes off len] feeds a chunk of input into [reader] for
      parsing. *)
  val feed : t -> bytes -> int -> int -> unit
end
