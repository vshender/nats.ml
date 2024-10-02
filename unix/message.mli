(** A module representing a message in the NATS protocol. *)

module Headers = Nats.Protocol.Headers

(** The type representing messages received from NATS. *)
type t = {
  subject : string;
  (** The subject to which the message was sent. *)
  reply : string option;
  (** An optional subject where replies to this message should be sent, if
      any. *)
  sid : int;
  (** The subscription ID associated with this message. *)
  headers : Headers.t option;
  (** Optional headers that may be included with the message. *)
  payload : string;
  (** The actual message payload, typically a string representing the data
      being communicated. *)
}
