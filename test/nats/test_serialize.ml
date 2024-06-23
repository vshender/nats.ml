(** Tests for NATS client protocol messages serializer. *)

open Alcotest

open Nats
open Nats.Protocol

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** [check_serializer msg cmd expected] is a testing function that serializes
    the NATS protocol message [cmd] and compares it with [expected].

    [msg] is printed if the check fails. *)
let check_serializer msg cmd expected = fun () ->
  let s = Faraday.create 256 in
  Serialize.serialize_client_message s cmd;
  Faraday.serialize_to_string s
  |> check string msg expected

(** Tests for the "CONNECT" protocol message serialization. *)
let connect_tests = "CONNECT", [
    "success" -: check_serializer "successfully serialized"
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
            ()))
      ({|CONNECT {"verbose":false,"pedantic":false,"tls_required":false,"auth_token":null,"user":null,"pass":null,"name":"","lang":"ocaml","version":"0.0.1","protocol":1,"echo":false,"signature":null,"jwt":null,"no_responders":false,"headers":true,"nkey":null}|} ^ "\r\n");
  ]

(** Tests for the "PUB" protocol message serialization. *)
let pub_tests = "PUB", [
    "empty payload" -: check_serializer "successfully serialized"
      (ClientMessage.Pub
         (Pub.make
            ~subject:"NOTIFY"
            ~payload:""
            ()))
      "PUB NOTIFY 0\r\n";
    "non-empty payload" -: check_serializer "successfully serialized"
      (ClientMessage.Pub
         (Pub.make
            ~subject:"FOO"
            ~payload:"Hello NATS!"
            ()))
      "PUB FOO 11\r\nHello NATS!\r\n";
    "reply-to" -: check_serializer "successfully serialized"
      (ClientMessage.Pub
         (Pub.make
            ~subject:"FRONT.DOOR"
            ~reply:"JOKE.22"
            ~payload:"Knock Knock"
            ()))
      "PUB FRONT.DOOR JOKE.22 11\r\nKnock Knock\r\n";
  ]

(** Tests for the "HPUB" protocol message serialization. *)
let hpub_tests = "HPUB", [
    "empty payload, no headers" -: check_serializer "successfully serialized"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"FOO"
            ~headers:(Headers.make ~headers:[] ())
            ~payload:""
            ()))
      "HPUB FOO 12 12\r\nNATS/1.0\r\n\r\n\r\n";
    "empty payload, one header" -: check_serializer "successfully serialized"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"NOTIFY"
            ~headers:(Headers.make ~headers:[("Bar", "Baz")] ())
            ~payload:""
            ()))
      "HPUB NOTIFY 22 22\r\nNATS/1.0\r\nBar: Baz\r\n\r\n\r\n";
    "non-empty payload, one header" -: check_serializer "successfully serialized"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"FOO"
            ~headers:(Headers.make ~headers:[("Bar", "Baz")] ())
            ~payload:"Hello NATS!"
            ()))
      "HPUB FOO 22 33\r\nNATS/1.0\r\nBar: Baz\r\n\r\nHello NATS!\r\n";
    "non-empty payload, one header, two values" -: check_serializer "successfully serialized"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"MORNING.MENU"
            ~headers:(Headers.make ~headers:[("BREAKFAST", "donut"); ("BREAKFAST", "eggs")] ())
            ~payload:"Yum!"
            ()))
      "HPUB MORNING.MENU 47 51\r\nNATS/1.0\r\nBREAKFAST: donut\r\nBREAKFAST: eggs\r\n\r\nYum!\r\n";
    "non-empty payload, two headers, reply-to" -: check_serializer "successfully serialized"
      (ClientMessage.HPub
         (HPub.make
            ~subject:"FRONT.DOOR"
            ~reply:"JOKE.22"
            ~headers:(Headers.make ~headers:[("BREAKFAST", "donut"); ("LUNCH", "burger")] ())
            ~payload:"Knock Knock"
            ()))
      "HPUB FRONT.DOOR JOKE.22 45 56\r\nNATS/1.0\r\nBREAKFAST: donut\r\nLUNCH: burger\r\n\r\nKnock Knock\r\n"
  ]

let sub_tests = "SUB", [
    "no queue group" -: check_serializer "successfully serialized"
      (ClientMessage.Sub
         (Sub.make
            ~subject:"FOO"
            ~sid:"1"
            ()))
      "SUB FOO 1\r\n";
    "queue group" -: check_serializer "successfully serialized"
      (ClientMessage.Sub
         (Sub.make
            ~subject:"BAR"
            ~group:"G1"
            ~sid:"44"
            ()))
      "SUB BAR G1 44\r\n";
  ]

let unsub_tests = "UNSUB", [
    "no max_msgs" -: check_serializer "successfully serialized"
      (ClientMessage.UnSub
         (UnSub.make
            ~sid:"1"
            ()))
      "UNSUB 1\r\n";
    "max_msgs" -: check_serializer "successfully serialized"
      (ClientMessage.UnSub
         (UnSub.make
            ~sid:"1"
            ~max_msgs:5
            ()))
      "UNSUB 1 5\r\n";
  ]

let ping_tests = "PING", [
    "success" -: check_serializer "successfully serialized"
      ClientMessage.Ping
      "PING\r\n";
  ]

let pong_tests = "PONG", [
    "success" -: check_serializer "successfully serialized"
      ClientMessage.Pong
      "PONG\r\n";
  ]

let () =
  run ~compact:true "Serialize" [
    connect_tests;
    pub_tests;
    hpub_tests;
    sub_tests;
    unsub_tests;
    ping_tests;
    pong_tests;
  ]
