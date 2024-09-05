(** Tests for NATS client protocol messages serializer. *)

open Alcotest

open Nats
open Nats.Protocol

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** [check_serializer msg expected cmd] is a testing function that serializes
    the NATS protocol message [cmd] and compares it with [expected].

    [msg] is printed if the check fails. *)
let check_serializer msg expected cmd = fun () ->
  let s = Faraday.create 256 in
  Serialize.serialize_client_message s cmd;
  Faraday.serialize_to_string s
  |> check string msg expected

let tests = "serialize", [
    (* Tests for the "CONNECT" protocol message serialization. *)
    "CONNECT: success" -: check_serializer "successfully serialized"
      ({|CONNECT {"verbose":false,"pedantic":false,"tls_required":false,"auth_token":null,"user":null,"pass":null,"name":"","lang":"ocaml","version":"0.0.1","protocol":1,"echo":false,"signature":null,"jwt":null,"no_responders":false,"headers":true,"nkey":null}|} ^ "\r\n")
      (ClientMessage.Connect
         (Connect.make
            ~verbose:false
            ~pedantic:false
            ~tls_required:false
            ~name:""
            ~lang:"ocaml"
            ~version:"0.0.1"
            ~protocol:1
            ~echo:false
            ~no_responders:false
            ~headers:true
            ()));

    (* Tests for the "PUB" protocol message serialization. *)
    "PUB: empty payload" -: check_serializer "successfully serialized"
      "PUB NOTIFY 0\r\n"
      (ClientMessage.Pub
         (Pub.make
            ~subject:"NOTIFY"
            ~payload:""
            ()));
    "PUB: non-empty payload" -: check_serializer "successfully serialized"
      "PUB FOO 11\r\nHello NATS!\r\n"
      (ClientMessage.Pub
         (Pub.make
            ~subject:"FOO"
            ~payload:"Hello NATS!"
            ()));
    "PUB: reply-to" -: check_serializer "successfully serialized"
      "PUB FRONT.DOOR JOKE.22 11\r\nKnock Knock\r\n"
      (ClientMessage.Pub
         (Pub.make
            ~subject:"FRONT.DOOR"
            ~reply:"JOKE.22"
            ~payload:"Knock Knock"
            ()));

    (* Tests for the "HPUB" protocol message serialization. *)
    "HPUB: empty payload, no headers" -: check_serializer "successfully serialized"
      "HPUB FOO 12 12\r\nNATS/1.0\r\n\r\n\r\n"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"FOO"
            ~headers:(Headers.make ~headers:[] ())
            ~payload:""
            ()));
    "HPUB: empty payload, one header" -: check_serializer "successfully serialized"
      "HPUB NOTIFY 22 22\r\nNATS/1.0\r\nBar: Baz\r\n\r\n\r\n"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"NOTIFY"
            ~headers:(Headers.make ~headers:[("Bar", "Baz")] ())
            ~payload:""
            ()));
    "HPUB: non-empty payload, one header" -: check_serializer "successfully serialized"
      "HPUB FOO 22 33\r\nNATS/1.0\r\nBar: Baz\r\n\r\nHello NATS!\r\n"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"FOO"
            ~headers:(Headers.make ~headers:[("Bar", "Baz")] ())
            ~payload:"Hello NATS!"
            ()));
    "HPUB: non-empty payload, one header, two values" -: check_serializer "successfully serialized"
      "HPUB MORNING.MENU 47 51\r\nNATS/1.0\r\nBREAKFAST: donut\r\nBREAKFAST: eggs\r\n\r\nYum!\r\n"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"MORNING.MENU"
            ~headers:(Headers.make ~headers:[("BREAKFAST", "donut"); ("BREAKFAST", "eggs")] ())
            ~payload:"Yum!"
            ()));
    "HPUB: non-empty payload, two headers, reply-to" -: check_serializer "successfully serialized"
      "HPUB FRONT.DOOR JOKE.22 45 56\r\nNATS/1.0\r\nBREAKFAST: donut\r\nLUNCH: burger\r\n\r\nKnock Knock\r\n"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"FRONT.DOOR"
            ~reply:"JOKE.22"
            ~headers:(Headers.make ~headers:[("BREAKFAST", "donut"); ("LUNCH", "burger")] ())
            ~payload:"Knock Knock"
            ()));

    (* Tests for the "SUB" protocol message serialization. *)
    "SUB: no queue group" -: check_serializer "successfully serialized"
      "SUB FOO 1\r\n"
      (ClientMessage.Sub
         (Sub.make
            ~subject:"FOO"
            ~sid:"1"
            ()));
    "SUB: queue group" -: check_serializer "successfully serialized"
      "SUB BAR G1 44\r\n"
      (ClientMessage.Sub
         (Sub.make
            ~subject:"BAR"
            ~group:"G1"
            ~sid:"44"
            ()));

    (* Tests for the "UNSUB" protocol message serialization. *)
    "UNSUB: no max_msgs" -: check_serializer "successfully serialized"
      "UNSUB 1\r\n"
      (ClientMessage.UnSub
         (UnSub.make
            ~sid:"1"
            ()));
    "UNSUB: max_msgs" -: check_serializer "successfully serialized"
      "UNSUB 1 5\r\n"
      (ClientMessage.UnSub
         (UnSub.make
            ~sid:"1"
            ~max_msgs:5
            ()));

    (* Tests for the "PING" protocol message serialization. *)
    "PING: success" -: check_serializer "successfully serialized"
      "PING\r\n"
      ClientMessage.Ping;

    (* Tests for the "PONG" protocol message serialization. *)
    "PONG: success" -: check_serializer "successfully serialized"
      "PONG\r\n"
      ClientMessage.Pong;
  ]
