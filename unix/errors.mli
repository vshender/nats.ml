(** NATS errors. *)

open Nats.Protocol

(** The type of NATS errors. *)
type nats_error =
  | NoServers                              (** No servers available for connection. *)
  | ConnectionClosed                       (** Connection closed. *)
  | SubscriptionClosed                     (** Subscription closed. *)
  | SyncSubRequired                        (** Illegal call on an async subscription. *)
  | AsyncSubRequired                       (** Illegal call on a sync subscription. *)
  | Timeout                                (** Timeout *)
  | NoInfoReceived                         (** Protocol error, INFO not received. *)
  | UnexpectedProtocol of ServerMessage.t  (** Got an unexpected protocol. *)
  | ProtocolError of string                (** Protocol message parsing error. *)
  | InvalidSubject                         (** Invalid subject. *)
  | StaleConnection                        (** Stale connection. *)
  | MaxConnectionsExceeded                 (** Server maximum connections exceeded. *)
  | PermissionsViolation of string         (** Permissions violation. *)
  | AuthorizationError                     (** Authorization error. *)
  | AuthenticationExpired                  (** Authentication expired. *)
  | AuthenticationRevoked                  (** Authentication revoked. *)
  | AccountAuthenticationExpired           (** Account authentication expired. *)
  | ServerError of string                  (** Server error. *)

(** [error_message err] return a string describing the given NATS error. *)
val error_message : nats_error -> string

(** [parse_server_err_msg err_msg] parses the argument of an ERR message. .*)
val parse_server_err_msg : string -> nats_error

(** [NatsError] is a NATS error exception. *)
exception NatsError of nats_error

(** [nats_error err] raises [(NatsError err)]. *)
val nats_error : nats_error -> 'a
