(** Request/reply example. *)

open Nats_unix

let main () =
  (* Create an unauthenticated connection to NATS. *)
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in

  (* Define a service function that will handle incoming requests.
     The function parses a delay value from the message payload, waits for that
     duration, and then sends a reply to the reply-to subject specified in the
     message. *)
  let service name msg =
    let n = Scanf.sscanf msg.Message.payload "request %f" Fun.id in
    Thread.delay n;
    Client.publish
      nc
      (Option.get msg.reply)
      (Printf.sprintf "%s: reply to %s" name msg.payload)
  in
  (* Create two queue subscriptions on the "request" subject with the same
     queue group. *)
  let _sub1 = Client.subscribe nc "request" ~group:"service"
      ~callback:(service "srv1")
  and _sub2 = Client.subscribe nc "request" ~group:"service"
      ~callback:(service "srv2")
  in

  (* Send a request to the "request" subject with a timeout of 1.5 seconds,
     that will be completed within one second. *)
  begin
    try
      let reply = Client.request nc "request" "request 1" ~timeout:1.5 in
      Printf.printf "reply 1: %s\n%!" reply.payload
    with NatsError Timeout ->
      Printf.printf "reply 1: timeout\n%!"
  end;

  (* Send a request to the "request" subject with a timeout of 1.5 seconds,
     that will be completed within two seconds. *)
  begin
    try
      let reply = Client.request nc "request" "request 2" ~timeout:1.5 in
      Printf.printf "reply 2: %s\n%!" reply.payload
    with NatsError Timeout ->
      Printf.printf "reply 2: timeout\n%!"
  end;

  (* Send a request to the "request" subject without a timeout, that will be
     completed within one second. *)
  begin
    try
      let reply = Client.request nc "request" "request 3" in
      Printf.printf "reply 3: %s\n%!" reply.payload
    with NatsError Timeout ->
      Printf.printf "reply 3: timeout\n%!"
  end

let () = main ()
