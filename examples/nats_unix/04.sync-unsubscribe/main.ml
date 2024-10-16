(** Unsubscribing from synchronous subscription example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Create a synchronous subscription on the "greet.*" wildcard. *)
  let sub = Client.subscribe nc "greet.*" in

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

  (* Unsubscribe from the "greet.*" wildcard. *)
  Subscription.unsubscribe sub;

  (* Publish a message after unsubscribe. *)
  Client.publish nc "greet.ted" "hello 4";

  (* Try to get the next message after unsubscribe. *)
  Printf.printf "getting the next message after unsubscribe...\n%!";
  begin try ignore (Subscription.next_msg sub ~timeout:1.) with
    | Failure e -> Printf.printf "%s\n%!" e  (* subscription is closed *)
  end

let () = main ()
