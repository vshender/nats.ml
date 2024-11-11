(** NATS client protocol messages parser tests. *)

open Alcotest

open Nats
open Nats.Protocol

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** The implementation of [Alcotest.testable] for [ServerMessage.t]. *)
let server_message = testable ServerMessage.pp ServerMessage.equal

(** [check_parser msg expected cmd] is a testing function that parses the NATS
    protocol message [cmd] and compares it with [expected].

    [msg] is printed if the check fails. *)
let check_parser msg expected cmd = fun () ->
  Angstrom.(parse_string ~consume:Consume.All Parse.server_message cmd)
  |> check (result server_message string) msg expected

let tests = "parse", [
    (* {{{ The "INFO" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "INFO: success" -: check_parser "successfully parsed"
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
            )))
      ({|INFO {"server_id":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","server_name":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","version":"2.10.16","proto":1,"git_commit":"80e29794","go":"go1.22.3","host":"0.0.0.0","port":4222,"headers":true,"max_payload":1048576,"client_id":5,"client_ip":"172.17.0.1","cluster":"my_cluster","xkey":"XDY6PRVVALMO27CBNH4EFRE4ULE6E6SP5XCUITGY366RNV7ZN4DEQQ6L"}|} ^ "\r\n");

    "INFO: success, char case" -: check_parser "successfully parsed"
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
            )))

      ({|iNfO {"server_id":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","server_name":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","version":"2.10.16","proto":1,"git_commit":"80e29794","go":"go1.22.3","host":"0.0.0.0","port":4222,"headers":true,"max_payload":1048576,"client_id":5,"client_ip":"172.17.0.1","cluster":"my_cluster","xkey":"XDY6PRVVALMO27CBNH4EFRE4ULE6E6SP5XCUITGY366RNV7ZN4DEQQ6L"}|} ^ "\r\n");

    "INFO: INFO expected" -: check_parser "INFO expected"
      (Error "INFO: INFO expected")
      "INF\r\n";

    "INFO: space expected" -: check_parser "space expected"
      (Error "INFO: space expected")
      "INFO\r\n";

    "INFO: malformed payload" -: check_parser "malformed payload"
      (Error "INFO: malformed payload")
      "INFO junk\r\n";

    "INFO: missing fields" -: check_parser "missing fields"
      (Error "INFO: malformed payload")
      ({|INFO {"server_id":"NBLWWR3GITDEO763HFYGJJBUUKVDHQNJPZ2TYIHSWPC2HBAILDKTHQ7P","max_payload":100,"auth_required":false}|} ^ "\r\n");

    (* }}} *)

    (* {{{ The "MSG" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "MSG: success" -: check_parser "successfully parsed"
      (Ok
         (ServerMessage.Msg
            (Msg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~payload:"Hello World"
               ()
            )))
      "MSG FOO.BAR 9 11\r\nHello World\r\n";

    "MSG: success, char case" -: check_parser "successfully parsed"
      (Ok
         (ServerMessage.Msg
            (Msg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~payload:"Hello World"
               ()
            )))
      "mSg FOO.BAR 9 11\r\nHello World\r\n";

    "MSG: success, reply-to" -: check_parser "successfully parsed"
      (Ok
         (ServerMessage.Msg
            (Msg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~reply:"GREETING.34"
               ~payload:"Hello World"
               ()
            )))
      "MSG FOO.BAR 9 GREETING.34 11\r\nHello World\r\n";

    "MSG: MSG expected" -: check_parser "MSG expected"
      (Error "MSG: MSG expected")
      "MS\r\n";

    "MSG: space expected" -: check_parser "space expected"
      (Error "MSG: space expected")
      "MSG\r\n";

    "MSG: wrong number of args" -: check_parser "wrong number of args"
      (Error "MSG: wrong number of args")
      "MSG A B C D E\r\nF\r\n";

    "MSG: size parsing error" -: check_parser "size parsing error"
      (Error "MSG: int_of_string")
      "MSG FOO.BAR 9 invalid_size\r\nHello World\r\n";

    "MSG: payload too short" -: check_parser "payload too short"
      (Error "MSG: not enough input")
      "MSG FOO.BAR 9 11\r\nHello\r\n";

    "MSG: payload too long" -: check_parser "payload too long"
      (Error "MSG: CRLF expected")
      "MSG FOO.BAR 9 5\r\nHello World\r\n";

    (* }}} *)

    (* {{{ The "HMSG" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "HMSG: success" -: check_parser "HMSG: success"
      (Ok
         (ServerMessage.HMsg
            (HMsg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~headers:(Headers.make
                           ~headers:[
                             ("FoodGroup", "vegetable");
                             ("FoodGroup", "plants");
                           ]
                           ())
               ~payload:"Hello World"
               ()
            )))
      "HMSG FOO.BAR 9 53 64\r\nNATS/1.0\r\nFoodGroup: vegetable\r\nFoodGroup: plants\r\n\r\nHello World\r\n";

    "HMSG: success, char case" -: check_parser "successfully parsed"
      (Ok
         (ServerMessage.HMsg
            (HMsg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~headers:(Headers.make
                           ~headers:[("FoodGroup", "vegetable")]
                           ())
               ~payload:"Hello World"
               ()
            )))
      "hMsG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    "HMSG: success, reply-to" -: check_parser "successfully parsed"
      (Ok
         (ServerMessage.HMsg
            (HMsg.make
               ~subject:"FOO.BAR"
               ~sid:"9"
               ~reply:"BAZ.69"
               ~headers:(Headers.make
                           ~headers:[("FoodGroup", "vegetable")]
                           ())
               ~payload:"Hello World"
               ()
            )))
      "hMsG FOO.BAR 9 BAZ.69 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    "HMSG: HMSG expected" -: check_parser "HMSG expected"
      (Error "HMSG: HMSG expected")
      "HMS\r\n";

    "HMSG: space expected" -: check_parser "space expected"
      (Error "HMSG: space expected")
      "HMSG\r\n";

    "HMSG: wrong number of args" -: check_parser "wrong number of args"
      (Error "HMSG: wrong number of args")
      "HMSG A B C D E F\r\nG\r\n";

    "HMSG: header size parsing error" -: check_parser "header size parsing error"
      (Error "HMSG: int_of_string")
      "HMSG FOO.BAR 9 invalid_header_size 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    "HMSG: total size parsing error" -: check_parser "total size parsing error"
      (Error "HMSG: int_of_string")
      "HMSG FOO.BAR 9 34 invalid_header_size\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    "HMSG: headers too short" -: check_parser "headers too short"
      (Error "HMSG: CRLF expected")
      "HMSG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: fruit\r\n\r\nHello World\r\n";

    "HMSG: headers too long" -: check_parser "headers too long"
      (Error "HMSG: CRLF expected")
      "HMSG FOO.BAR 9 30 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    "HMSG: payload too short" -: check_parser "payload too short"
      (Error "HMSG: not enough input")
      "HMSG FOO.BAR 9 34 45\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello\r\n";

    "HMSG: payload too long" -: check_parser "payload too long"
      (Error "HMSG: CRLF expected")
      "HMSG FOO.BAR 9 34 39\r\nNATS/1.0\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    "HMSG: invalid headers" -: check_parser "HMSG: invalid headers"
      (Error "HMSG: invalid headers")
      "HMSG FOO.BAR 9 45 56\r\nNATS/1.0\r\nFoodGroup: vegetable\r\nFoodGroup\r\n\r\nHello World\r\n";

    "HMSG: invalid version" -: check_parser "HMSG: invalid version"
      (Error "HMSG: invalid headers")
      "HMSG FOO.BAR 9 BAZ.69 34 45\r\nNATS/x.y\r\nFoodGroup: vegetable\r\n\r\nHello World\r\n";

    (* }}} *)

    (* {{{ The "PING" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "PING: success" -: check_parser "successfully parsed"
      (Ok ServerMessage.Ping)
      "PING\r\n";

    "PING: success, char case" -: check_parser "successfully parsed"
      (Ok ServerMessage.Ping)
      "pInG\r\n";

    "PING: PING expected" -: check_parser "PING expected"
      (Error "PING: PING expected")
      "PIN\r\n";

    "PING: CRLF expected" -: check_parser "CRLF expected"
      (Error "PING: CRLF expected")
      "PING  \r\n";

    (* }}} *)

    (* {{{ The "PONG" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "PONG: success" -: check_parser "successfully parsed"
      (Ok ServerMessage.Pong)
      "PONG\r\n";

    "PONG: success, char case" -: check_parser "successfully parsed"
      (Ok ServerMessage.Pong)
      "pong\r\n";

    "PONG: PONG expected" -: check_parser "PONG expected"
      (Error "PONG: PONG expected")
      "PON\r\n";

    "PONG: CRLF expected" -: check_parser "CRLF expected"
      (Error "PONG: CRLF expected")
      "PONG  \r\n";

    (* }}} *)

    (* {{{ The "+OK" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "OK: success" -: check_parser "successfully parsed"
      (Ok ServerMessage.Ok)
      "+OK\r\n";

    "OK: success, char case" -: check_parser "successfully parsed"
      (Ok ServerMessage.Ok)
      "+oK\r\n";

    "OK: OK expected" -: check_parser "OK expected"
      (Error "OK: +OK expected")
      "+O\r\n";

    "OK: CRLF expected" -: check_parser "CRLF expected"
      (Error "OK: CRLF expected")
      "+OK  \r\n";

    (* }}} *)

    (* {{{ The "-ERR" protocol message parsing tests.
       ------------------------------------------------------------------------
    *)

    "ERR: success" -: check_parser "successfully parsed"
      (Ok (ServerMessage.Err "Unknown Protocol Operation"))
      "-ERR 'Unknown Protocol Operation'\r\n";

    "ERR: success" -: check_parser "successfully parsed"
      (Ok (ServerMessage.Err "Stale Connection"))
      "-ERR 'Stale Connection'\r\n";

    "ERR: success, char case" -: check_parser "successfully parsed"
      (Ok (ServerMessage.Err "Unknown Protocol Operation"))
      "-eRr 'Unknown Protocol Operation'\r\n";

    "ERR: ERR expected" -: check_parser "ERR expected"
      (Error "ERR: -ERR expected")
      "-ER 'Unknown Protocol Operation'\r\n";

    "ERR: single quote expected" -: check_parser "single quote expected"
      (Error "ERR: single quote expected")
      "-ERR Unknown Protocol Operation\r\n";

    "ERR: single quote expected" -: check_parser "single quote expected"
      (Error "ERR: single quote expected")
      "-ERR 'Unknown Protocol Operation\r\n";

    "ERR: single quote expected" -: check_parser "single quote expected"
      (Error "ERR: single quote expected")
      "-ERR \"Unknown Protocol Operation\"\r\n";

    "ERR: CRLF expected" -: check_parser "CLRF expected"
      (Error "ERR: CRLF expected")
      "-ERR 'Unknown Protocol Operation'";

    (* }}} *)

    (* {{{ Various failure tests.
       ------------------------------------------------------------------------
    *)

    "not enough input" -: check_parser "not enough input"
      (Error "NATS: not enough input")
      "";

    "not enough input" -: check_parser "not enough input"
      (Error "NATS: not enough input")
      "P";

    "unknown protocol" -: check_parser "unknown protocol"
      (Error "NATS: unknown protocol")
      "?";

    "unknown protocol" -: check_parser "unknown protocol"
      (Error "NATS: unknown protocol")
      "UNKNOWN";

    "unknown protocol" -: check_parser "unknown protocol"
      (Error "NATS: unknown protocol")
      "PUNKNOWN";

    (* }}} *)
  ]
