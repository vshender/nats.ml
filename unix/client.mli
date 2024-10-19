(** The NATS client module. *)

(** The type of NATS client. *)
type t

(** The type of callback functions for handling income messages *)
type callback = Message.t -> unit

(** The type of callback functions for handling errors. *)
type error_callback = exn -> unit

(** [connect ?url ?name ?verbose ?pedantic ?connect_timeout ?keepalive ?error_cb ?inbox_prefix ()]
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
    - [ing_interval] (optional): the period (in seconds) at which the client
      will be sending PING commands to the server.  Defaults to 120 seconds.
    - [error_cb] (optional): a callback function to report errors.
    - [inbox_prefix] (optional): a custom prefix for inbox subjects.
*)
val connect :
  ?url:string ->
  ?name:string ->
  ?verbose:bool ->
  ?pedantic:bool ->
  ?ping_interval:float ->
  ?connect_timeout:float ->
  ?error_cb:error_callback ->
  ?inbox_prefix:string ->
  unit -> t

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

    Raises [Failure] if the connection is closed. *)
val subscribe : t -> ?group:string -> ?callback:callback -> string -> Subscription.t

(** [publish t ?reply subject payload] publishes a message to the given subject
    with an optional reply-to subject.

    Raises [Failure] if the connection is closed. *)
val publish : t -> ?reply:string -> string -> string -> unit

(** [request t ?timeout subject payload] sends a request message to the given
    subject and waits for a reply within an optional timeout period.

    Raises [Failure] if the connection is closed. *)
val request : t -> ?timeout:float -> string -> string -> Message.t option

(** [flush ?timeout t] performs a round trip to the server and returns when it
    receives the internal reply, or if the call times-out ([timeout] is
    expressed in seconds).

    This function ensures that all messages sent to the server have been
    processed.  This is useful to ensure message delivery before proceeding
    with further actions.

    Raises [Failure] if the connection is closed or if the flush operation
    times out. *)
val flush : ?timeout:float -> t -> unit

(** [close t] closes the connection to the NATS server.

    Raises [Failure] if the connection is already closed. *)
val close : t -> unit

(** [drain t] safely closes the connection after ensuring all buffered messages
    have been sent and processed by subscriptions.

    Raises [Failure] if the connection is already closed or if the drain
    operation times out. *)
val drain : ?timeout:float -> t -> unit
