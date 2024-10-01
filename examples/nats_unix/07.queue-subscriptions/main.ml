(** Queue subscriptions example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Create two queue subscriptions on the "greet.*" wildcard with the same
     queue group. *)
  let _sub1 = Client.subscribe nc "greet.*" ~group:"test"
      ~callback:(fun msg ->
          Printf.printf "sub1: msg data: %s on subject %s\n%!"
            msg.payload msg.subject)
  and _sub2 = Client.subscribe nc "greet.*" ~group:"test"
      ~callback:(fun msg ->
          Printf.printf "sub2: msg data: %s on subject %s\n%!"
            msg.payload msg.subject)
  in

  (* Publish messages.  These messages will be routed to one of the subscribers
     in the "test" group. *)
  let names = [
    "joe"; "pam"; "bob"; "ted"; "ada";
    "ian"; "mia"; "leo"; "zoe"; "max";
  ] in
  List.iteri
    (fun i name ->
       Client.publish
         nc (Printf.sprintf "greet.%s" name) (Printf.sprintf "hello %d" i))
    names;

  (* Drain is a safe way to ensure all buffered messages that were published
     are sent and all buffered messages received on a subscription are
     processed before closing the connection. *)
  Client.drain nc

let () = main ()
