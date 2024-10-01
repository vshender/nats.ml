(** Simple synchronous NATS client.

    Usage example:

    {[
      # open Nats_unix ;;
      # let nc = Client.connect () ;;
      val nc : t = <abstr>
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # let sub = Client.subscribe nc "test" ~callback:(fun msg -> Printf.printf "Message recieved: %s\n%!" msg.payload) ;;
      val sub : Subscription.t = <abstr>
      # Client.publish nc "test" "Hello World" ;;
      Message received: Hello World
      - : unit = ()
      # Subscription.unsubscribe sub ;;
      - : unit = ()
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # let sub = Client.subscribe nc "test2" ;;
      val sub : Subscription.t = <abstr>
      # Client.publish nc "test2" "Hello World" ;;
      - : unit = ()
      # Subscription.next_msg ~timeout:2. sub ;;
      - : Msg.t option =
      Some
       {Nats.Protocol.Msg.subject = "test2"; reply = None; sid = "2";
        payload = "Hello World"}
      # Subscription.next_msg ~timeout:2. sub ;;
      - : Msg.t option = None
      # Subscription.unsubscribe sub ;;
      - : unit = ()
      # Subscription.next_msg ~timeout:2. sub ;;
      Exception: Failure "subscription is closed".
      # Client.close nc ;;
      - : unit = ()
    ]}
*)

module Headers = Message.Headers

module Message = Message

module Client = Client

module Subscription : sig
  type t = Subscription.t

  val sid : t -> int
  val subject : t -> string
  val group : t -> string option
  val delivered : t -> int
  val is_sync : t -> bool
  val is_closed : t -> bool
  val unsubscribe : ?max_msgs:int -> t -> unit
  val next_msg : ?timeout:float -> t -> Message.t option
end
