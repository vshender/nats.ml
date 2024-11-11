(** The NATS errors. *)

open Nats.Protocol

(** The type of NATS errors. *)
type t =
  | NoServers                              (** No servers available for connection. *)
  | ConnectionClosed                       (** Attempt to perform an operation on a closed connection. *)
  | ConnectionLost                         (** Connection lost during an ongoing operation. *)
  | SubscriptionClosed                     (** Subscription closed. *)
  | SyncSubRequired                        (** Illegal call on an async subscription. *)
  | AsyncSubRequired                       (** Illegal call on a sync subscription. *)
  | Timeout                                (** Timeout. *)
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
  | MessageCallbackError of exn            (** An error occurred in the message callback. *)

(** [equal e1 e2] is [true] if the errors [e1] and [e2] are equal. *)
val equal : t -> t -> bool

(** [pp fmt e] pretty-prints the error [e]. *)
val pp : Format.formatter -> t -> unit

(** [show e] is a string representation of the error [e]. *)
val show : t -> string

(** [error_message err] is a human-readable string describing the given error
    [err]. *)
val error_message : t -> string

(** [parse_server_err_msg err_msg] parses the argument of an [-ERR] message. *)
val parse_server_err_msg : string -> t

(** [NatsError] is a NATS error exception. *)
exception NatsError of t

(** [nats_error err] raises a [NatsError] exception with the given error
    [err]. *)
val nats_error : t -> 'a
