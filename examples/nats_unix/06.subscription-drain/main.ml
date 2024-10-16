(** Draining a subscription example. *)

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

  (* Publish several messages. *)
  Client.publish nc "greet.joe" "hello 1";
  Client.publish nc "greet.pam" "hello 2";
  Client.publish nc "greet.bob" "hello 3";

  (* Draining a subscription ensures that buffered messages that were published
     are sent and all buffered messages received on the subsciption are
     processed before closing the subscription. *)
  Subscription.drain sub;

  (* Close the connection to NATS.  We don't need to drain the client because
     we only had a single subscription and already drained it. *)
  Client.close nc

let () = main ()
