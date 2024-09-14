(** Simple synchronous NATS client.

    Usage example:

    {[
      # open Nats_unix ;;
      # let nc = Client.connect () ;;
      val nc : t = <abstr>
      # Client.read_msg ~timeout:2. nc ;;
      - : Nats.Protocol.ServerMessage.t option = Some Nats.Protocol.ServerMessage.Ping
      # Client.send_msg nc (Nats.Protocol.ClientMessage.Pong) ;;
      - : unit = ()
      # Client.read_msg ~timeout:2. nc ;;
      - : Nats.Protocol.ServerMessage.t option = None
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # Client.read_msg ~timeout:2. nc ;;
      - : Nats.Protocol.ServerMessage.t option = None
      # Client.subscribe nc "test" ;;
      - : unit = ()
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # Client.read_msg ~timeout:2. nc ;;
      - : Nats.Protocol.ServerMessage.t option =
      Some
       (Nats.Protocol.ServerMessage.Msg
         {Nats.Protocol.Msg.subject = "test"; reply = None; sid = "1"; payload = "Hello World"})
    ]}
*)

module Client = Client
