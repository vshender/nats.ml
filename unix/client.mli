(** NATS client. *)

type t

type callback = Message.t -> unit
type error_callback = exn -> unit

val connect :
  ?url:string ->
  ?name:string ->
  ?verbose:bool ->
  ?pedantic:bool ->
  ?connect_timeout:float ->
  ?keepalive:bool ->
  ?error_cb:error_callback ->
  unit -> t

val send_msg : t -> Nats.Protocol.ClientMessage.t -> unit

val subscribe : t -> ?group:string -> ?callback:callback -> string -> Subscription.t

val publish : t -> string -> string -> unit

val flush : t -> unit

val close : t -> unit

val drain : t -> unit
