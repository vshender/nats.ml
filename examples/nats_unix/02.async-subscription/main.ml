(** Asynchronous subscription example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Messages are published to subjects.  Although there are no subscribers,
     this will be published successfully. *)
  Client.publish nc "greet.joe" "hello";

  (* Create an asynchronous subscription on the "greet.*" wildcard. *)
  let _sub = Client.subscribe nc "greet.*"
      ~callback:(fun msg ->
          Printf.printf "msg data: %s on subject %s\n%!" msg.payload msg.subject)
  in

  (* Publish a couple messages. *)
  Client.publish nc "greet.joe" "hello 1";
  Client.publish nc "greet.pam" "hello 2";

  (* Publish one more message. *)
  Client.publish nc "greet.bob" "hello 3";

  (* Drain is a safe way to ensure all buffered messages that were published
     are sent and all buffered messages received on a subscription are
     processed before closing the connection. *)
  Client.drain nc

let () = main ()
