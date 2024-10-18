(** Serialization of NATS client protocol messages produced by a client.

    See {{:https://docs.nats.io/reference/reference-protocols/nats-protocol} Client Protocol}
    for details.
*)

(** [serialize_client_message serializer msg] serializes the NATS client
    protocol message [msg] into [serializer]. *)
val serialize_client_message : Faraday.t -> Protocol.ClientMessage.t -> unit
