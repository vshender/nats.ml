(** Reply to a message example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Create a new unique subject (inbox) that will be used to receive replies. *)
  let reply_to = Client.new_inbox nc in

  (* Create two subscriptions with the same queue group to handle incoming
     requests on the [request] subject.  This means they will distribute the
     messages among themselves (load balancing).  The subscribers will reply to
     any message they receive by publishing a message to the subject specified
     in the [reply] field of the received message. *)
  let sub1 = Client.subscribe nc "request" ~group:"service"
      ~callback:(fun msg ->
          Client.publish
            nc
            (Option.get msg.reply)
            (Printf.sprintf "srv1: reply to %s" msg.payload))
  and sub2 = Client.subscribe nc "request" ~group:"service"
      ~callback:(fun msg ->
          Client.publish
            nc
            (Option.get msg.reply)
            (Printf.sprintf "srv2: reply to %s" msg.payload))

  (* Subscribe to the [reply_to] inbox to receive the replies. *)
  and _sub3 = Client.subscribe nc reply_to
      ~callback:(fun msg ->
          Printf.printf "reply received: %s\n%!" msg.payload)
  in

  (* Publish messages, specifying [reply_to] as the reply subject. *)
  Client.publish nc ~reply:reply_to "request" "request 1";
  Client.publish nc ~reply:reply_to "request" "request 2";
  Client.publish nc ~reply:reply_to "request" "request 3";
  Client.publish nc ~reply:reply_to "request" "request 4";
  Client.publish nc ~reply:reply_to "request" "request 5";
  Client.publish nc ~reply:reply_to "request" "request 6";

  (* Flush the connection to ensure all published requests are sent to the
     server and received by the subscribers. *)
  Client.flush nc;

  (* Wait until all messages are processed by subscribers and responses are
     sent to the server. *)
  Subscription.drain sub1;
  Subscription.drain sub2;

  (* Drain the connection to ensure all buffered messages received on
     asynchronous subscriptions are processed before closing the connection. *)
  Client.drain nc

let () = main ()
