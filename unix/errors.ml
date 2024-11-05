(** NATS errors. *)

open Nats.Protocol

type nats_error =
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
  (* | BadQueueName             (\** Invalid queue name. *\) *)
  (* | ErrSlowConsumer          (\** Slow consumer, messages dropped. *\) *)
  (* | BadTimeout               (\** Invalid timeout. *\) *)
  (* | JsonParse                (\** Connect message, JSON parse error. *\) *)
  (* | NoEchoNotSupported       (\** No echo option not supported by this server. *\) *)
  (* | ClientIDNotSupported     (\** Client ID not supported by this server *\) *)
  (* | MsgNotBound              (\** Message is not bound to subscription/connection. *\) *)
  (* | MsgNoReply               (\** Message does not have a reply. *\) *)
  (* | Disconnected             (\** Server is disconnected. *\) *)
  (* | NoResponders             (\** No responders available for request. *\) *)

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
  (* | BadQueueName           -> "nats: invalid queue name" *)
  (* | ErrSlowConsumer        -> "nats: slow consumer, messages dropped" *)
  (* | BadTimeout             -> "nats: invalid timeout" *)
  (* | JsonParse              -> "nats: connect message, JSON parse error" *)
  (* | NoEchoNotSupported     -> "nats: no echo option not supported by this server" *)
  (* | ClientIDNotSupported   -> "nats: client ID not supported by this server" *)
  (* | MsgNotBound            -> "nats: message is not bound to subscription/connection" *)
  (* | MsgNoReply             -> "nats: message does not have a reply" *)
  (* | Disconnected           -> "nats: server is disconnected" *)
  (* | NoResponders           -> "nats: no responders available for request" *)

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

exception NatsError of nats_error

let nats_error err = raise (NatsError err)
