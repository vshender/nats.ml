(** Unsubscribing after N messages example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Create an asynchronous subscription on the "greet.*" wildcard. *)
  let sub = Client.subscribe nc "greet.*"
      ~callback:(fun msg ->
          Thread.delay 1.;  (* simulate message processing delay *)
          Printf.printf "msg data: %s on subject %s\n%!" msg.payload msg.subject)
  in

  (* Unsubscribe from the "greet.*" wildcard after receiving 3 messages. *)
  Subscription.unsubscribe sub ~max_msgs:3;

  (* Publish messages. *)
  Client.publish nc "greet.joe" "hello 1";
  Client.publish nc "greet.pam" "hello 2";
  Client.publish nc "greet.bob" "hello 3";
  Client.publish nc "greet.ted" "hello 4";

  (* Drain is a safe way to ensure all buffered messages that were published
     are sent and all buffered messages received on asynchronous subscriptions
     are processed before closing the connection. *)
  Client.drain nc

let () = main ()
