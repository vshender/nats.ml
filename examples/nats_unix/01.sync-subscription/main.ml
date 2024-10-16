(** Synchronous subscription example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Messages are published to subjects.  Although there are no subscribers,
     this will be published successfully. *)
  Client.publish nc "greet.joe" "hello";

  (* Create a synchronous subscription on the "greet.*" wildcard. *)
  let sub = Client.subscribe nc "greet.*" in

  (* For a synchronous subscription, we need to fetch the next message.
     However, since the publish occured before the subscription was
     established, this is going to timeout. *)
  let msg = Subscription.next_msg sub ~timeout:1. in
  Printf.printf "subscribed after a publish...\n%!";
  Printf.printf "msg is nil? %b\n%!" (Option.is_none msg);

  (* Publish a couple messages. *)
  Client.publish nc "greet.joe" "hello 1";
  Client.publish nc "greet.pam" "hello 2";

  (* Since the subscription is established, the published messages will
     immediately be broadcasted to all subscriptions.  They will land in
     their buffer for subsequent [next_msg] calls. *)
  let msg = Subscription.next_msg sub ~timeout:1. in
  begin match msg with
    | Some msg -> Printf.printf "msg data: %s on subject %s\n%!" msg.payload msg.subject
    | None     -> assert false
  end;

  let msg = Subscription.next_msg sub ~timeout:1. in
  begin match msg with
    | Some msg -> Printf.printf "msg data: %s on subject %s\n%!" msg.payload msg.subject
    | None     -> assert false
  end;

  (* One more for good measures... *)
  Client.publish nc "greet.bob" "hello 3";

  let msg = Subscription.next_msg sub ~timeout:1. in
  begin match msg with
    | Some msg -> Printf.printf "msg data: %s on subject %s\n%!" msg.payload msg.subject
    | None     -> assert false
  end

let () = main ()
