(** Tests for NATS client protocol messages parser. *)

open Alcotest

open Nats
open Nats.Protocol

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** The implementation of [Alcotest.testable] for [ServerMessage.t]. *)
let server_message = testable ServerMessage.pp ServerMessage.equal

(** [check_parser msg cmd expected] is a testing function that parses the NATS
    protocol message [cmd] and compares it with [expected].

    [msg] is printed if the check fails. *)
let check_parser msg cmd expected = fun () ->
  Angstrom.(parse_string ~consume:Consume.All Parse.server_message cmd)
  |> check (result server_message string) msg expected

(** Tests for the "INFO" protocol message parsing. *)
let info_tests = "INFO", [
    "success" -: check_parser "successfully parsed"
      ({|INFO {"server_id":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","server_name":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","version":"2.10.16","proto":1,"git_commit":"80e29794","go":"go1.22.3","host":"0.0.0.0","port":4222,"headers":true,"max_payload":1048576,"client_id":5,"client_ip":"172.17.0.1","cluster":"my_cluster","xkey":"XDY6PRVVALMO27CBNH4EFRE4ULE6E6SP5XCUITGY366RNV7ZN4DEQQ6L"}|} ^ "\r\n")
      (Ok
         (ServerMessage.Info
            (Info.make
               ~server_id:"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P"
               ~server_name:"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P"
               ~version:"2.10.16"
               ~proto:1
               ~git_commit:"80e29794"
               ~go:"go1.22.3"
               ~host:"0.0.0.0"
               ~port:4222
               ~headers:true
               ~max_payload:1048576L
               ~client_id:5L
               ~client_ip:"172.17.0.1"
               ~cluster:"my_cluster"
               ()
            )));
    "success, char case" -: check_parser "successfully parsed"
      ({|iNfO {"server_id":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","server_name":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","version":"2.10.16","proto":1,"git_commit":"80e29794","go":"go1.22.3","host":"0.0.0.0","port":4222,"headers":true,"max_payload":1048576,"client_id":5,"client_ip":"172.17.0.1","cluster":"my_cluster","xkey":"XDY6PRVVALMO27CBNH4EFRE4ULE6E6SP5XCUITGY366RNV7ZN4DEQQ6L"}|} ^ "\r\n")
      (Ok
         (ServerMessage.Info
            (Info.make
               ~server_id:"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P"
               ~server_name:"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P"
               ~version:"2.10.16"
               ~proto:1
               ~git_commit:"80e29794"
               ~go:"go1.22.3"
               ~host:"0.0.0.0"
               ~port:4222
               ~headers:true
               ~max_payload:1048576L
               ~client_id:5L
               ~client_ip:"172.17.0.1"
               ~cluster:"my_cluster"
               ()
            )));
    "INFO expected" -: check_parser "INFO expected"
      "INF\r\n"
      (Error "INFO: INFO expected");
    "space expected" -: check_parser "space expected"
      "INFO\r\n"
      (Error "INFO: space expected");
    "malformed payload" -: check_parser "malformed payload"
      "INFO junk\r\n"
      (Error "INFO: malformed payload");
    "missing fields" -: check_parser "missing fields"
      ({|INFO {"server_id":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","max_payload":100,"auth_required":false}|} ^ "\r\n")
      (Error "INFO: malformed payload");
  ]

(** Tests for the "MSG" protocol message parsing. *)
let msg_tests = "MSG", [
    "success" -: check_parser "successfully parsed"
      "MSG FOO.BAR 9 11\r\nHello World\r\n"
      (Ok
         (ServerMessage.Msg
            (Msg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~payload:"Hello World"
               ()
            )));
    "success, char case" -: check_parser "successfully parsed"
      "mSg FOO.BAR 9 11\r\nHello World\r\n"
      (Ok
         (ServerMessage.Msg
            (Msg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~payload:"Hello World"
               ()
            )));
    "success, reply-to" -: check_parser "successfully parsed"
      "MSG FOO.BAR 9 GREETING.34 11\r\nHello World\r\n"
      (Ok
         (ServerMessage.Msg
            (Msg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~reply:"GREETING.34"
               ~payload:"Hello World"
               ()
            )));
    "MSG expected" -: check_parser "MSG expected"
      "MS\r\n"
      (Error "MSG: MSG expected");
    "space expected" -: check_parser "space expected"
      "MSG\r\n"
      (Error "MSG: space expected");
    "wrong number of args" -: check_parser "wrong number of args"
      "MSG A B C D E\r\nF\r\n"
      (Error "MSG: wrong number of args");
    "size parsing error" -: check_parser "size parsing error"
      "MSG FOO.BAR 9 invalid_size\r\nHello World\r\n"
      (Error "MSG: int_of_string");
    "payload too short" -: check_parser "payload too short"
      "MSG FOO.BAR 9 11\r\nHello\r\n"
      (Error "MSG: not enough input");
    "payload too long" -: check_parser "payload too long"
      "MSG FOO.BAR 9 5\r\nHello World\r\n"
      (Error "MSG: CRLF expected");
  ]

(** Tests for the "HMSG" protocol message parsing. *)
let hmsg_tests = "HMSG", [
    "success" -: check_parser "successfully parsed"
      "HMSG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Ok
         (ServerMessage.HMsg
            (HMsg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~headers:"NATS/1.0\r\nFoodGroup: vegetable"
               ~payload:"Hello World"
               ()
            )));
    "success, char case" -: check_parser "successfully parsed"
      "hMsG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Ok
         (ServerMessage.HMsg
            (HMsg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~headers:"NATS/1.0\r\nFoodGroup: vegetable"
               ~payload:"Hello World"
               ()
            )));
    "success, reply-to" -: check_parser "successfully parsed"
      "hMsG FOO.BAR 9 BAZ.69 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Ok
         (ServerMessage.HMsg
            (HMsg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~reply:"BAZ.69"
               ~headers:"NATS/1.0\r\nFoodGroup: vegetable"
               ~payload:"Hello World"
               ()
            )));
    "HMSG expected" -: check_parser "HMSG expected"
      "HMS\r\n"
      (Error "HMSG: HMSG expected");
    "space expected" -: check_parser "space expected"
      "HMSG\r\n"
      (Error "HMSG: space expected");
    "wrong number of args" -: check_parser "wrong number of args"
      "HMSG A B C D E F\r\nG\r\n"
      (Error "HMSG: wrong number of args");
    "header size parsing error" -: check_parser "header size parsing error"
      "HMSG FOO.BAR 9 invalid_header_size 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Error "HMSG: int_of_string");
    "total size parsing error" -: check_parser "total size parsing error"
      "HMSG FOO.BAR 9 34 invalid_header_size\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Error "HMSG: int_of_string");
    "headers too short" -: check_parser "headers too short"
      "HMSG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: fruit\r\n\r\nHello World\r\n"
      (Error "HMSG: CRLF expected");
    "headers too long" -: check_parser "headers too long"
      "HMSG FOO.BAR 9 30 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Error "HMSG: CRLF expected");
    "payload too short" -: check_parser "payload too short"
      "HMSG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello\r\n"
      (Error "HMSG: not enough input");
    "payload too long" -: check_parser "payload too long"
      "HMSG FOO.BAR 9 34 39\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n"
      (Error "HMSG: CRLF expected");
  ]

(** Tests for the "PING" protocol message parsing. *)
let ping_tests = "PING", [
    "success" -: check_parser "successfully parsed"
      "PING\r\n"
      (Ok ServerMessage.Ping);
    "success, char case" -: check_parser "successfully parsed"
      "pInG\r\n"
      (Ok ServerMessage.Ping);
    "PING expected" -: check_parser "PING expected"
      "PIN\r\n"
      (Error "PING: PING expected");
    "CRLF expected" -: check_parser "CRLF expected"
      "PING  \r\n"
      (Error "PING: CRLF expected");
  ]

(** Tests for the "PONG" protocol message parsing. *)
let pong_tests = "PONG", [
    "success" -: check_parser "successfully parsed"
      "PONG\r\n"
      (Ok ServerMessage.Pong);
    "success, char case" -: check_parser "successfully parsed"
      "pong\r\n"
      (Ok ServerMessage.Pong);
    "PONG expected" -: check_parser "PONG expected"
      "PON\r\n"
      (Error "PONG: PONG expected");
    "CRLF expected" -: check_parser "CRLF expected"
      "PONG  \r\n"
      (Error "PONG: CRLF expected");
  ]

(** Tests for the "+OK" protocol message parsing. *)
let ok_tests = "OK", [
    "success" -: check_parser "successfully parsed"
      "+OK\r\n"
      (Ok ServerMessage.Ok);
    "success, char case" -: check_parser "successfully parsed"
      "+oK\r\n"
      (Ok ServerMessage.Ok);
    "OK expected" -: check_parser "OK expected"
      "+O\r\n"
      (Error "OK: +OK expected");
    "CRLF expected" -: check_parser "CRLF expected"
      "+OK  \r\n"
      (Error "OK: CRLF expected");
  ]

(** Tests for the "-ERR" protocol message parsing. *)
let err_tests = "ERR", [
    "success" -: check_parser "successfully parsed"
      "-ERR 'Unknown Protocol Operation'\r\n"
      (Ok (ServerMessage.Err "Unknown Protocol Operation"));
    "success" -: check_parser "successfully parsed"
      "-ERR 'Stale Connection'\r\n"
      (Ok (ServerMessage.Err "Stale Connection"));
    "success, char case" -: check_parser "successfully parsed"
      "-eRr 'Unknown Protocol Operation'\r\n"
      (Ok (ServerMessage.Err "Unknown Protocol Operation"));
    "ERR expected" -: check_parser "ERR expected"
      "-ER 'Unknown Protocol Operation'\r\n"
      (Error "ERR: -ERR expected");
    "single quote expected" -: check_parser "single quote expected"
      "-ERR Unknown Protocol Operation\r\n"
      (Error "ERR: single quote expected");
    "single quote expected" -: check_parser "single quote expected"
      "-ERR 'Unknown Protocol Operation\r\n"
      (Error "ERR: single quote expected");
    "single quote expected" -: check_parser "single quote expected"
      "-ERR \"Unknown Protocol Operation\"\r\n"
      (Error "ERR: single quote expected");
    "CRLF expected" -: check_parser "CLRF expected"
      "-ERR 'Unknown Protocol Operation'"
      (Error "ERR: CRLF expected");
  ]

(** Various failure tests. *)
let fail_tests = "failure", [
    "not enough input" -: check_parser "not enough input"
      ""
      (Error "NATS: not enough input");
    "not enough input" -: check_parser "not enough input"
      "P"
      (Error "NATS: not enough input");
    "unknown protocol" -: check_parser "unknown protocol"
      "?"
      (Error "NATS: unknown protocol");
    "unknown protocol" -: check_parser "unknown protocol"
      "UNKNOWN"
      (Error "NATS: unknown protocol");
    "unknown protocol" -: check_parser "unknown protocol"
      "PUNKNOWN"
      (Error "NATS: unknown protocol");
  ]

let () =
  run ~compact:true "Parse" [
    info_tests;
    msg_tests;
    hmsg_tests;
    ping_tests;
    pong_tests;
    ok_tests;
    err_tests;
    fail_tests;
  ]
