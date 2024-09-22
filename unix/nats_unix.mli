(** Simple synchronous NATS client.

    Usage example:

    {[
      # open Nats_unix ;;
      # open Nats.Protocol ;;
      # let nc = Client.connect () ;;
      val nc : t = <abstr>
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # Client.subscribe nc "test" (fun msg -> Printf.printf "Message recieved: %s\n%!" msg.Msg.payload) ;;
      - : unit = ()
      # Client.publish nc "test" "Hello World" ;;
      - : unit = ()
      # Client.close nc ;;
    ]}
*)

module Client = Client
