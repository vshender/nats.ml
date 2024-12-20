(** The NATS client module. *)

(** The type of NATS client. *)
type t

(** The type of callback functions for handling income messages. *)
type callback = Nats.Message.t -> unit

(** The type of callback functions used for asynchronous connection events such
    as closed connections. *)
type conn_callback = t -> unit

(** The type of callback functions used to process asynchronous errors
    encountered while processing inbound messages. *)
type error_callback = t -> Nats.Errors.t -> unit

(** [connect ?url ?name ?verbose ?pedantic ?connect_timeout ?ping_interval ?max_pings_outstanding ?closed_cb ?error_cb ?inbox_prefix ()]
    establishes a connection to a NATS server.

    - [url] (optional): the URL of the NATS server (default is
      "nats://127.0.0.1:4222").
    - [name] (optional): label the connection with name (shown in NATS
      monitoring).
    - [verbose] (optional): turns on [+OK] protocol acknowledgements (turned
      off by default).
    - [pedantic] (optional): enables pedantic mode for stricter protocol checks
      (turned off by default).
    - [connect_timeout] (optional): specifies the connection timeout in
      seconds.
    - [ping_interval] (optional): the period (in seconds) at which the client
      will be sending PING commands to the server.  Defaults to 120 seconds.
    - [max_pings_outstanding] (optional): the maximum number of pending PING
      commands that can be awaiting a response before raising a
      [StaleConnection] error.  Defaults to 2.
    - [closed_cb] (optional): a callback function that is called when the
      client is no longer connected to a server.
    - [error_cb] (optional): a callback function to report asynchronous errors.
    - [inbox_prefix] (optional): a custom prefix for inbox subjects.

    Raises:

    - [NatsError NoServers] if the connection was refused.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
    - [NatsError Timeout] if the connection times out.
    - [NatsError NoInfoReceived] if no INFO message was received from the
      server.
    - [NatsError (ProtocolError e)] if there was an error parsing a server
      message.
*)
val connect :
  ?url:string ->
  ?name:string ->
  ?verbose:bool ->
  ?pedantic:bool ->
  ?connect_timeout:float ->
  ?ping_interval:float ->
  ?max_pings_outstanding:int ->
  ?closed_cb:conn_callback ->
  ?error_cb:error_callback ->
  ?inbox_prefix:string ->
  unit -> t

(** [with_client ?url ?name ?verbose ?pedantic ?connect_timeout ?ping_interval ?max_pings_outstanding ?closed_cb ?error_cb ?inbox_prefix f]
    establishes a connection to a NATS server, executes the function [f] with
    the connected client, and then safely drains and closes the connection when
    [f] completes or raises an exception.

    - [url] (optional): the URL of the NATS server (default is
      "nats://127.0.0.1:4222").
    - [name] (optional): label the connection with name (shown in NATS
      monitoring).
    - [verbose] (optional): turns on [+OK] protocol acknowledgements (turned
      off by default).
    - [pedantic] (optional): enables pedantic mode for stricter protocol checks
      (turned off by default).
    - [connect_timeout] (optional): specifies the connection timeout in
      seconds.
    - [ping_interval] (optional): the period (in seconds) at which the client
      will be sending PING commands to the server.  Defaults to 120 seconds.
    - [max_pings_outstanding] (optional): the maximum number of pending PING
      commands that can be awaiting a response before raising a
      [StaleConnection] error.  Defaults to 2.
    - [closed_cb] (optional): a callback function that is called when the
      client is no longer connected to a server.
    - [error_cb] (optional): a callback function to report asynchronous errors.
    - [inbox_prefix] (optional): a custom prefix for inbox subjects.
    - [f]: the function to execute with the connected client.

    Raises:

    - [NatsError NoServers] if the connection was refused.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
    - [NatsError Timeout] if the connection times out.
    - [NatsError NoInfoReceived] if no INFO message was received from the
      server.
    - [NatsError (ProtocolError e)] if there was an error parsing a server
      message.
    - Any exception thrown by the user-supplied function [f].
*)
val with_client :
  ?url:string ->
  ?name:string ->
  ?verbose:bool ->
  ?pedantic:bool ->
  ?connect_timeout:float ->
  ?ping_interval:float ->
  ?max_pings_outstanding:int ->
  ?closed_cb:conn_callback ->
  ?error_cb:error_callback ->
  ?inbox_prefix:string ->
  (t -> 'a) -> 'a

(** [is_closed c] is [true] if the connection closed. *)
val is_closed : t -> bool

(** [last_error t] is the last error encountered via the connection.  It can be
    used reliably within [closed_cb] in order to find out the reason why the
    connection was closed for example. *)
val last_error : t -> Nats.Errors.t option

(** [new_inbox t] returns a unique inbox that can be used for NATS requests or
    subscriptions. *)
val new_inbox : t -> string

(** [subscribe t ?group ?callback subject] expresses interest in the given
    [subject].

    The subject can have wildcards.  There are two type of wildcards: [*] for
    partial, and [>] for full.

    A subscription on subject [time.*.east] would receive messages sent to
    [time.us.east] and [time.eu.east].  A subscription on subject [time.us.>]
    would receive messages sent to [time.us.east] and [time.us.east.atlanta],
    while [time.us.*] would only match [time.us.east] since [*] can't match
    more than one token.

    If [callback] is given, the subscription is asynchronous and messages will
    be delivered to [callback].  [callback] will be called in a separate
    message processing thread.

    If [callback] is not given, the subscription is synchronous and
    the {!Subscription.next_msg} function is used to obtain the delivered
    messages.

    If [queue] is given the subscription is a queue subscription.  All
    subscribers with the same queue name will form the queue group and only one
    member of the group will be selected to receive any given message.

    Raises [NatsError ConnectionClosed] if the connection is closed. *)
val subscribe : t -> ?group:string -> ?callback:callback -> string -> Subscription.t

(** [publish t ?reply subject payload] publishes a message to the given subject
    with an optional reply-to subject.

    Raises [NatsError ConnectionClosed] if the connection is closed. *)
val publish : t -> ?reply:string -> string -> string -> unit

(** [request t ?timeout subject payload] sends a request message to the given
    subject and waits for a reply within an optional timeout period.

    Raises:

    - [NatsError ConnectionClosed] if the connection is closed.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
    - [NatsError Timeout] if the request times out.
*)
val request : t -> ?timeout:float -> string -> string -> Nats.Message.t

(** [request_opt t ?timeout subject payload] sends a request message to the
    given subject and waits for a reply within an optional timeout period.
    Returns [None] if the request times out.

    Raises:

    - [NatsError ConnectionClosed] if the connection is closed.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
*)
val request_opt : t -> ?timeout:float -> string -> string -> Nats.Message.t option

(** [flush ?timeout t] performs a round trip to the server and returns when it
    receives the internal reply, or if the call times-out ([timeout] is
    expressed in seconds).

    This function ensures that all messages sent to the server have been
    processed.  This is useful to ensure message delivery before proceeding
    with further actions.

    Raises:

    - [NatsError ConnectionClosed] if the connection is closed.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
    - [NatsError Timeout] if the flush operation times out.
*)
val flush : ?timeout:float -> t -> unit

(** [close t] closes the connection to the NATS server.

    Raises [NatsError ConnectionClosed] if the connection is already closed. *)
val close : t -> unit

(** [drain t] safely closes the connection after ensuring all buffered messages
    have been sent and processed by subscriptions.

    Raises:

    - [NatsError ConnectionClosed] if the connection is already closed.
    - [NatsError ConnectionLost] if the connection is lost during the
      operation.
    - [NatsError Timeout] if the drain operation times out.
*)
val drain : ?timeout:float -> t -> unit
