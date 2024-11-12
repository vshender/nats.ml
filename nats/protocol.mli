(** NATS client protocol messages.

    See {{:https://docs.nats.io/reference/reference-protocols/nats-protocol} Client Protocol}
    for details.
*)

(** The NATS message headers module. *)
module Headers : sig
  (** The type of NATS message headers.

      See {{:https://github.com/nats-io/nats-architecture-and-design/blob/main/adr/ADR-4.md}NATS Message Headers}
      for details. *)
  type t = {
    version: int * int;               (** The header version. *)
    headers: (string * string) list;  (** The headers themselves. *)
  }

  (** [make ?version ?headers ()] creates a new headers record with an optional
      [version] and [headers]. *)
  val make : ?version:int * int -> ?headers:(string * string) list -> unit -> t

  (** [equal h1 h2] is [true] if [h1] and [h2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt h] pretty-prints [h]. *)
  val pp : Format.formatter -> t -> unit

  (** [show h] is a string representation of [h]. *)
  val show : t -> string

  (** [version h] returns the version of [h]. *)
  val version : t -> int * int

  (** [get key h] returns the first value found matching [key] in a
      case-sensitive lookup in [h].

      Returns [None] if a matching [key] is not found.  *)
  val get : string -> t -> string option

  (** [values key h] returns a list of all values that case-sensitive match
      [key] in [h]. *)
  val values : string -> t -> string list

  (** [iter f h] applies [f] in turn to each key-value pair in [h]. *)
  val iter : (string -> string -> unit) -> t -> unit

  (** [fold f h init] computes [(f kN vN ... (f k2 v2 (f k1 v1 init)) ...)],
      where [(k1, v1), ..., (kN, vN)] are the elements of [h].  Each header is
      presented exactly once to [f]. *)
  val fold : (string -> string -> 'acc -> 'acc) -> t -> 'acc -> 'acc
end

(** The module representing the INFO message arguments. *)
module Info : sig
  (** The type of the INFO message argument. *)
  type t = {
    server_id : string;
    (** The unique identifier of the NATS server. *)
    server_name : string;
    (** The name of the NATS server. *)
    version : string;
    (** The version of NATS. *)
    go : string;
    (** The version of golang the NATS server was built with. *)
    host : string;
    (** The IP address used to start the NATS server. *)
    port : int;
    (** The port number the NATS server is configured to listen on. *)
    headers : bool;
    (** Whether the server supports headers. *)
    max_payload : Int64.t;
    (** Maximum payload size, in bytes, that the server will accept from the
        client. *)
    proto : int;
    (** An integer indicating the protocol version of the server. *)
    client_id : Int64.t option;
    (** The internal client identifier in the server. *)
    auth_required : bool;
    (** If this is true, then the client should try to authenticate upon
        connect. *)
    tls_required : bool;
    (** If this is true, then the client must perform the TLS/1.2 handshake. *)
    tls_verify : bool;
    (** If this is true, the client must provide a valid certificate during the
        TLS handshake. *)
    tls_available : bool;
    (** If this is true, the client can provide a valid certificate during the
        TLS handshake. *)
    connect_urls : string array;
    (** List of server urls that a client can connect to. *)
    ws_connect_urls : string array;
    (** List of server urls that a websocket client can connect to. *)
    lame_duck_mode : bool;
    (** If the server supports /Lame Duck Mode/ notifications, and the current
        server has transitioned to lame duck, [lame_duck_mode] will be set to
        [true]. *)
    git_commit : string option;
    (** The git hash at which the NATS server was built. *)
    jetstream : bool;
    (** Whether the server supports JetStream. *)
    ip : string option;
    (** The IP of the server. *)
    client_ip : string option;
    (** The IP of the client. *)
    nonce : string option;
    (** The nonce for use in CONNECT. *)
    cluster : string option;
    (** The name of the cluster. *)
    domain : string option;
    (** The configured NATS domain of the server. *)
  }

  (** [make] is a constructor for the [Info.t] records. *)
  val make :
    server_id:string ->
    server_name:string ->
    version:string ->
    go:string ->
    host:string ->
    port:int ->
    headers:bool ->
    max_payload:int64 ->
    proto:int ->
    ?client_id:int64 ->
    ?auth_required:bool ->
    ?tls_required:bool ->
    ?tls_verify:bool ->
    ?tls_available:bool ->
    ?connect_urls:string array ->
    ?ws_connect_urls:string array ->
    ?lame_duck_mode:bool ->
    ?git_commit:string ->
    ?jetstream:bool ->
    ?ip:string ->
    ?client_ip:string ->
    ?nonce:string ->
    ?cluster:string ->
    ?domain:string ->
    unit -> t

  (** [of_yojson json] parses [Info.t] from JSON. *)
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

(** The module representing the CONNECT message arguments. *)
module Connect : sig
  (** The type of the CONNECT message argument. *)
  type t = {
    verbose : bool;
    (** Turns on [+OK] protocol acknowledgements. *)
    pedantic : bool;
    (** Turns on additional strict format checking, e.g. for properly formed
        subjects. *)
    tls_required : bool;
    (** Indicates whether the client requires an SSL connection. *)
    auth_token : string option;
    (** Client authorization token. *)
    user : string option;
    (** Connection username. *)
    pass : string option;
    (** Connection password. *)
    name : string option;
    (** Client name. *)
    lang : string;
    (** The implementation language of the client. *)
    version : string;
    (** The version of the client. *)
    protocol : int;
    (** Sending [0] (or absent) indicates client supports original protocol.
        Sending [1] indicates that the client supports dynamic reconfiguration
        of cluster topology changes by asynchronously receiving [INFO] messages
        with known servers it can reconnect to. *)
    echo : bool;
    (** If set to [false], the server (version 1.2.0+) will not send
        originating messages from this connection to its own subscriptions.
        Clients should set this to [false] only for server supporting this
        feature, which is when proto in the [INFO] protocol is set to at least
        [1]. *)
    signature : string option;
    (** In case the server has responded with a [nonce] on [INFO], then a NATS
        client must use this field to reply with the signed [nonce]. *)
    jwt : string option;
    (** The JWT that identifies a user permissions and account. *)
    no_responders : bool;
    (** Enable quick replies for cases where a request is sent to a topic with
        no responders. *)
    headers : bool;
    (** Whether the client supports headers. *)
    nkey : string option;
    (** The public NKey to authenticate the client.  This will be used to
        verify the signature ([sig]) against the [nonce] provided in the [INFO]
        message. *)
  }

  (** [make] is a constructor for the [Connect.t] records. *)
  val make :
    verbose:bool ->
    pedantic:bool ->
    tls_required:bool ->
    ?auth_token:string ->
    ?user:string ->
    ?pass:string ->
    ?name:string ->
    lang:string ->
    version:string ->
    protocol:int ->
    echo:bool ->
    ?signature:string ->
    ?jwt:string ->
    no_responders:bool ->
    headers:bool ->
    ?nkey:string ->
    unit -> t

  (** [to_yojson connect] is a JSON representation of [connect]. *)
  val to_yojson : t -> Yojson.Safe.t
end

(** The module representing the PUB message arguments. *)
module Pub : sig
  (** The type of the PUB message arguments.*)
  type t = {
    subject : string;         (** The subject name. *)
    reply   : string option;  (** An optional reply subject name. *)
    payload : string;         (** The message payload *)
  }

  (** [make] is a constructor for the [Pub.t] records. *)
  val make : subject:string -> ?reply:string -> payload:string -> unit -> t
end

(** The module representing the HPUB message arguments. *)
module HPub : sig
  (** The type of the HPUB message arguments. *)
  type t = {
    subject : string;         (** The subject name. *)
    reply   : string option;  (** An optional reply subject name. *)
    headers : Headers.t;      (** The message headers. *)
    payload : string;         (** The message payload. *)
  }

  (** [make] is a constructor for the [HPub.t] records. *)
  val make :
    subject:string ->
    ?reply:string ->
    headers:Headers.t ->
    payload:string ->
    unit -> t
end

(** The module representing the SUB message arguments. *)
module Sub : sig
  (** The type of the SUB message arguments. *)
  type t = {
    subject : string;         (** The subject name. *)
    group   : string option;  (** An optional queue group. *)
    sid     : string;         (** The subscription ID. *)
  }

  (** [make] is a constructor for the [Sub.t] records. *)
  val make : subject:string -> ?group:string -> sid:string -> unit -> t
end

(** The module representing the UNSUB message arguments. *)
module UnSub : sig
  (** The type of the UNSUB message arguments. *)
  type t = {
    sid : string;
    (** The subscription ID *)
    max_msgs : int option;
    (** The number of messages to wait for before automatically
        unsubscribing. *)
  }

  (** [make] is a constructor for the [UnSub.t] records. *)
  val make : sid:string -> ?max_msgs:int -> unit -> t
end

(** The module representing the MSG message arguments. *)
module Msg : sig
  (** The type of the MSG message arguments. *)
  type t = {
    subject : string;         (** The subject name. *)
    reply   : string option;  (** An optional reply subject name. *)
    sid     : string;         (** The subscription ID. *)
    payload : string;         (** The message payload. *)
  }

  (** [make] is a constructor for the [Msg.t] records. *)
  val make :
    subject:string ->
    ?reply:string ->
    sid:string ->
    payload:string ->
    unit -> t
end

(** The module represeenting the HMSG arguments. *)
module HMsg : sig
  (** The type of the HMSG message arguments. *)
  type t = {
    subject : string;         (** The subject name.  *)
    reply   : string option;  (** An optional reply subject name. *)
    sid     : string;         (** The subscription ID. *)
    headers : Headers.t;      (** The message headers. *)
    payload : string;         (** The message payload. *)
  }

  (** [make] is a constructor for the [HMsg.t] records. *)
  val make :
    subject:string ->
    ?reply:string ->
    sid:string ->
    headers:Headers.t ->
    payload:string ->
    unit -> t
end

(** The module representing the NATS client protocol messages produced by
    a client. *)
module ClientMessage : sig
  (** The type of protocol messages produced by a client. *)
  type t =
    | Connect of Connect.t
    (** Sent to a server to specify connection information. *)
    | Pub of Pub.t
    (** Publish a message to a subject, with optional reply subject. *)
    | HPub of HPub.t
    (** Publish a message to a subject including NATS headers, with optional
        reply subject. *)
    | Sub of Sub.t
    (** Subscribe to a subject (or subject wildcard). *)
    | UnSub of UnSub.t
    (** Unsubscribe (or auto-unsubscribe) from a subject. *)
    | Ping
    (** PING keep-alive message. *)
    | Pong
    (** PONG keep-alive message. *)

  (** [equal msg1 msg2] is [true] if [msg1] and [msg2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt msg] pretty-prints [msg]. *)
  val pp : Format.formatter -> t -> unit

  (** [show msg] is a string representation of [msg]. *)
  val show : t -> string
end

(** The module representing the NATS client protocol messages produced by
    a server. *)
module ServerMessage : sig
  (** The type of protocol messages produced by a server. *)
  type t =
    | Info of Info.t
    (** Sent to a client after initial TCP/IP connection. *)
    | Msg of Msg.t
    (** Delivers a message payload to a subscriber. *)
    | HMsg of HMsg.t
    (** Delivers a message payload to a subscriber with NATS headers. *)
    | Ping
    (** PING keep-alive message. *)
    | Pong
    (** PONG keep-alive message. *)
    | Ok
    (** Acknowledges well-formed protocol message in [verbose] mode. *)
    | Err of string
    (** Indicates a protocol error; may cause client disconnect. *)

  (** [equal msg1 msg2] is [true] if [msg1] and [msg2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt msg] pretty-prints [msg]. *)
  val pp : Format.formatter -> t -> unit

  (** [show msg] is a string representation of [msg]. *)
  val show : t -> string
end
