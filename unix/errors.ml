(** The NATS errors. *)

open Nats.Protocol

(** [pp_exn fmt exn] pretty-prints an exception [exn] *)
let pp_exn fmt exn = Format.fprintf fmt "%s" @@ Printexc.to_string exn

(** [equal_exn exn1 exn2] checks whether two exceptions [exn1] and [exn2] are
    equal. *)
let equal_exn = ( = )

type t =
  | NoServers
  | ConnectionClosed
  | ConnectionLost
  | SubscriptionClosed
  | SyncSubRequired
  | AsyncSubRequired
  | Timeout
  | NoInfoReceived
  | UnexpectedProtocol of ServerMessage.t
  | ProtocolError of string
  | InvalidSubject
  | StaleConnection
  | MaxConnectionsExceeded
  | PermissionsViolation of string
  | AuthorizationError
  | AuthenticationExpired
  | AuthenticationRevoked
  | AccountAuthenticationExpired
  | ServerError of string
  | MessageCallbackError of exn
[@@deriving eq, show { with_path = false }]

let error_message = function
  | NoServers                    -> "no servers available for connection"
  | ConnectionClosed             -> "connection closed"
  | ConnectionLost               -> "connection lost"
  | SubscriptionClosed           -> "subscription closed"
  | SyncSubRequired              -> "illegal call on an async subscription"
  | AsyncSubRequired             -> "illegal call on a sync subscription"
  | Timeout                      -> "timeout"
  | NoInfoReceived               -> "no info received"
  | UnexpectedProtocol msg       -> Printf.sprintf "unexpected protocol: %s" @@ ServerMessage.show msg
  | ProtocolError e              -> Printf.sprintf "protocol error: %s" e
  | InvalidSubject               -> "invalid subject"
  | StaleConnection              -> "stale connection"
  | MaxConnectionsExceeded       -> "server maximum connections exceeded"
  | PermissionsViolation e       -> e
  | AuthorizationError           -> "authorization error"
  | AuthenticationExpired        -> "authentication expired"
  | AuthenticationRevoked        -> "authentication revoked"
  | AccountAuthenticationExpired -> "account authentication expired"
  | ServerError e                -> e
  | MessageCallbackError exn     -> Printf.sprintf "message callback error: %s" @@ Printexc.to_string exn

let parse_server_err_msg err_msg =
  match String.lowercase_ascii err_msg with
  | "invalid subject" ->
    InvalidSubject
  | "stale connection" ->
    StaleConnection
  | "maximum connections exceeded" ->
    MaxConnectionsExceeded
  | e when String.starts_with e ~prefix:"permissions violation" ->
    PermissionsViolation e
  | e when String.starts_with e ~prefix:"authorization violation" ->
    AuthorizationError
  | e when String.starts_with e ~prefix:"user authentication expired" ->
    AuthenticationExpired
  | e when String.starts_with e ~prefix:"user authentication revoked" ->
    AuthenticationRevoked
  | e when String.starts_with e ~prefix:"account authentication expired" ->
    AccountAuthenticationExpired
  | e ->
    ServerError e

exception NatsError of t

let nats_error err = raise (NatsError err)
