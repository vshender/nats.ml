(** The module representing a message in the NATS protocol. *)

module Headers = Protocol.Headers

type t = {
  subject : string;
  reply   : string option;
  sid     : int;
  headers : Headers.t option;
  payload : string;
}
[@@deriving eq, make, show { with_path = false }]
