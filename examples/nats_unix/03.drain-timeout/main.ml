(** A connection draining timeout example.  *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Create an asynchronous subscription on the "greet.*" wildcard. *)
  let _sub = Client.subscribe nc "greet.*"
      ~callback:(fun msg ->
          Thread.delay 1.;  (* simulate message processing delay *)
          Printf.printf "msg data: %s on subject %s\n%!" msg.payload msg.subject)
  in

  (* Publish several messages. *)
  Client.publish nc "greet.joe" "hello 1";
  Client.publish nc "greet.pam" "hello 2";
  Client.publish nc "greet.bob" "hello 3";
  Client.publish nc "greet.ted" "hello 4";

  (* Attempt to drain the subscription with a timeout of 2 seconds.  If
     draining is not completed within the timeout, handle the failure.

     In the case of timeout, [drain] waits for the current message being
     processed to complete processing, if any, so if the message processing
     loop started processing a message before the draining timeout occurred,
     it will complete that work. *)
  begin
    try
      Client.drain nc ~timeout:2.;
    with
      NatsError Timeout -> Printf.printf "connection draining timeout\n%!"
  end

let () = main ()
