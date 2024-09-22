(** NATS client. *)

open Nats.Protocol

type t

type callback = Msg.t -> unit
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

(* val read_msg : ?timeout:float -> t -> ServerMessage.t option *)

val send_msg : t -> Nats.Protocol.ClientMessage.t -> unit

val subscribe : t -> ?group:string -> string -> callback -> unit

val publish : t -> string -> string -> unit

val flush : t -> unit

val close : t -> unit
