(** The module representing a message in the NATS protocol. *)

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

(** [make] is a constructor for the message records. *)
val make :
  subject:string ->
  ?reply:string ->
  sid:int ->
  ?headers:Headers.t ->
  payload:string ->
  unit -> t

(** [equal msg1 msg2] is [true] if [msg1] and [msg2] are equal. *)
val equal : t -> t -> bool

(** [pp fmt msg] pretty-prints [msg]. *)
val pp : Format.formatter -> t -> unit

(** [show msg] is a string representation of [msg]. *)
val show : t -> string
