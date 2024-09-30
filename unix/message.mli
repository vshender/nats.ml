module Headers = Nats.Protocol.Headers

type t = {
  subject : string;
  reply   : string option;
  sid     : int;
  headers : Headers.t option;
  payload : string;
}
