(** A [Unix] library based client for NATS. *)

module Errors = Errors

type nats_error = Errors.t

exception NatsError = Errors.NatsError

module Headers = Message.Headers

module Message = Message

module Client = Client

module Subscription = Subscription

(* Ignore the SIGPIPE signal to prevent the program from terminating when
   attempting to write to a closed socket. *)
let _ = Sys.(signal sigpipe Signal_ignore);
