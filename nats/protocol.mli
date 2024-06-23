(** NATS client protocol messages.

    See {{:https://docs.nats.io/reference/reference-protocols/nats-protocol} Client Protocol}
    for details.
*)

module Info : sig
  (** A type of the INFO message argument. *)
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

  (** [of_yojson json] parses [Info.t] from a JSON representation. *)
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

(** A module containing a type of the CONNECT message argument and functions
    for working with it. *)
module Connect : sig
  (** A type of the CONNECT message argument. *)
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

(** A module containing a type of the PUB message arguments and functions for
    working with it. *)
module Pub : sig
  (** A type of the PUB message arguments.*)
  type t = {
    subject : string;         (** subject name *)
    reply   : string option;  (** reply subject *)
    payload : string;         (** message payload *)
  }

  (** [make] is a constructor for the [Pub.t] records. *)
  val make : subject:string -> ?reply:string -> payload:string -> unit -> t
end

(** A module containing a type of the HPUB message arguments and functions for
    working with it. *)
module HPub : sig
  (** A type of the HPUB message arguments. *)
  type t = {
    subject : string;         (** subject name *)
    reply   : string option;  (** reply subject *)
    headers : string;         (** message headers *)
    payload : string;         (** message payload *)
  }

  (** [make] is a constructor for the [HPub.t] records. *)
  val make :
    subject:string ->
    ?reply:string ->
    headers:string ->
    payload:string ->
    unit -> t
end

(** A module containing a type of the SUB message arguments and functions for
    working with it. *)
module Sub : sig
  (** A type of the SUB message arguments. *)
  type t = {
    subject : string;         (** subject name *)
    group   : string option;  (** queue group *)
    sid     : string;         (** subscription ID *)
  }

  (** [make] is a constructor for the [Sub.t] records. *)
  val make : subject:string -> ?group:string -> sid:string -> unit -> t
end

(** A module containing a type of the UNSUB message arguments and functions for
    working with it. *)
module UnSub : sig
  (** A type of the UNSUB message arguments. *)
  type t = {
    sid      : string;
    (** subscription ID *)
    max_msgs : int option;
    (** number of messages to wait for before automatically unsubscribing *)
  }

  (** [make] is a constructor for the [UnSub.t] records. *)
  val make : sid:string -> ?max_msgs:int -> unit -> t
end

(** A module containing a type of the MSG message arguments and functions for
    working with it. *)
module Msg : sig
  (** A type of the MSG message arguments. *)
  type t = {
    subject : string;         (** subject name *)
    reply   : string option;  (** subject name for reply *)
    sid     : string;         (** subscription ID *)
    payload : string;         (** message payload *)
  }

  (** [make] is a constructor for the [Msg.t] records. *)
  val make :
    subject:string ->
    ?reply:string ->
    sid:string ->
    payload:string ->
    unit -> t
end

(** A module containing a type of the HMSG message arguments and functions for
    working with it. *)
module HMsg : sig
  (** A type of the HMSG message arguments. *)
  type t = {
    subject : string;         (** subject name  *)
    reply   : string option;  (** subject name for reply *)
    sid     : string;         (** subscription ID *)
    headers : string;         (** message headers *)
    payload : string;         (** message payload *)
  }

  (** [make] is a constructor for the [HMsg.t] records. *)
  val make :
    subject:string ->
    ?reply:string ->
    sid:string ->
    headers:string ->
    payload:string ->
    unit -> t
end

(** A module containing a type of NATS client protocol messages sending by
    a client and functions for working with it. *)
module ClientMessage : sig
  (** A type of protocol messages sending by client. *)
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

(** A module containing a type of NATS client protocol messages sending by
    a server and functions for working with it. *)
module ServerMessage : sig
  (** A type of protocol messages sending by server. *)
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
