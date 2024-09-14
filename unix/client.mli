(** NATS client. *)

open Nats.Protocol

type t

val connect :
  ?url:string ->
  ?name:string ->
  ?verbose:bool ->
  ?pedantic:bool ->
  ?connect_timeout:float ->
  ?keepalive:bool ->
  unit -> t

val read_msg : ?timeout:float -> t -> ServerMessage.t option

val send_msg : t -> Nats.Protocol.ClientMessage.t -> unit

val subscribe : t -> ?group:string -> string -> unit

val publish : t -> string -> string -> unit

val close : t -> int
