type callback = Message.t -> unit

type t

val sid : t -> int

val subject : t -> string

val group : t -> string option

val is_sync : t -> bool

val create :
  ?unsubscribe_cb : (t -> unit) ->
  ?next_msg_start_cb : (t -> float option -> unit) ->
  ?next_msg_finish_cb : (t -> unit) ->
  int -> string -> string option -> callback option -> t

val close : t -> unit

val unsubscribe : t -> unit

val signal_timeout : t -> unit

val handle_msg : t -> Message.t -> unit

val next_msg : ?timeout:float -> t -> Message.t option
