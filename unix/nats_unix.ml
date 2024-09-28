(** Simple synchronous NATS client. *)

module Client = Client

module Subscription = Subscription

(* Ignore the SIGPIPE signal to prevent the program from terminating when
   attempting to write to a closed socket. *)
let _ = Sys.(signal sigpipe Signal_ignore);
