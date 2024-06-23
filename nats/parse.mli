(** Parsing of NATS client protocol messages sending by a server.

    See {{:https://docs.nats.io/reference/reference-protocols/nats-protocol} Client Protocol}
    for details.
*)

(** [server_message] parses a NATS client protocol message sending by server. *)
val server_message : Protocol.ServerMessage.t Angstrom.t
