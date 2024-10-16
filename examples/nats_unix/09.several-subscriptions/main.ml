(** Several subscriptions example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Create two subscriptions on the "greet.*" wildcard. *)
  let _sub1 = Client.subscribe nc "greet.*"
      ~callback:(fun msg ->
          Printf.printf "sub1: msg data: %s on subject %s\n%!"
            msg.payload msg.subject)
  and _sub2 = Client.subscribe nc "greet.*"
      ~callback:(fun msg ->
          Printf.printf "sub2: msg data: %s on subject %s\n%!"
            msg.payload msg.subject)
  in

  (* Publish messages.  Both subscriptions will receive each of these
     messages, and both callbacks will be executed for each message. *)
  Client.publish nc "greet.joe" "hello 1";
  Client.publish nc "greet.pam" "hello 2";
  Client.publish nc "greet.bob" "hello 3";

  (* Drain is a safe way to ensure all buffered messages that were published
     are sent and all buffered messages received on asynchronous subscriptions
     are processed before closing the connection. *)
  Client.drain nc

let () = main ()
