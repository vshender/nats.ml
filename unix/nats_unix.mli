(** Simple synchronous NATS client.

    Usage example:

    {[
      # open Nats_unix ;;
      # open Nats.Protocol ;;
      # let nc = Client.connect () ;;
      val nc : t = <abstr>
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # Client.subscribe nc ~callback:(fun msg -> Printf.printf "Message recieved: %s\n%!" msg.Msg.payload) "test" ;;
      - : Subscription.t = <abstr>
      # Client.publish nc "test" "Hello World" ;;
      Message received: Hello World
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
      # Client.close nc ;;
      - : unit = ()
    ]}
*)

module Client = Client

module Subscription : sig
  type t = Subscription.t

  val next_msg : ?timeout:float -> t -> Nats.Protocol.Msg.t option
end
