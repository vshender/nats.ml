(** The NATS client tests. *)

open Alcotest

open Nats.Protocol
open Nats_server
open Nats_unix

open Util

let tests ns = "NATS client", [
    (* {{{ The [connect] function tests.
       ------------------------------------------------------------------------
    *)

    "connect: success (real server)" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc
    end;

    "connect: success (real server, verbose)" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) ~verbose:true () in
      Client.close nc
    end;

    "connect: success (fake server)" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
        ]
      in

      let nc = Client.connect ~url:(FakeServer.client_url ns) () in
      Client.close nc
    end;

    "connect: success (fake server, verbose)" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "+OK\r\n";
          `Msg "PONG\r\n";
        ]
      in

      let nc = Client.connect ~url:(FakeServer.client_url ns) ~verbose:true () in
      Client.close nc
    end;

    "connect: no servers" -: begin fun () ->
      check_raises
        "No servers"
        (NatsError NoServers)
        (fun () -> ignore @@ Client.connect ~url:"nats://127.0.0.1:1" ())
    end;

    "connect: timeout" -: begin fun () ->
      let ns = FakeServer.run [`Delay 0.5] in

      check_raises
        "Timeout"
        (NatsError Timeout)
        (fun () -> ignore @@
          Client.connect ~url:(FakeServer.client_url ns) ~connect_timeout:0.2 ())
    end;

    "connect: no info" -: begin fun () ->
      let ns = FakeServer.run [`Msg "PING\r\n"] in

      check_raises
        "No info received"
        (NatsError NoInfoReceived)
        (fun () -> ignore @@ Client.connect ~url:(FakeServer.client_url ns) ())
    end;

    "connect: lost connection" -: begin fun () ->
      let ns = FakeServer.run [`Msg info_msg] in

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> ignore @@ Client.connect ~url:(FakeServer.client_url ns) ())
    end;

    "connect: error" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "-ERR 'Maximum Connections Exceeded'\r\n";
        ]
      in

      check_raises
        "Error received"
        (NatsError MaxConnectionsExceeded)
        (fun () -> ignore @@ Client.connect ~url:(FakeServer.client_url ns) ())
    end;

    "connect: unexpected protocol" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "+OK\r\n";
        ]
      in

      check_raises
        "Unexpected protocol received"
        (NatsError (UnexpectedProtocol (ServerMessage.Ok)))
        (fun () -> ignore @@ Client.connect ~url:(FakeServer.client_url ns) ())
    end;

    "connect: unexpected protocol (verbose)" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "+OK\r\n";
          `Msg "+OK\r\n";
        ]
      in

      check_raises
        "Unexpected protocol received"
        (NatsError (UnexpectedProtocol (ServerMessage.Ok)))
        (fun () -> ignore @@
          Client.connect ~url:(FakeServer.client_url ns) ~verbose:true ())
    end;

    "connect: unknown protocol" -: begin fun () ->
      let ns = FakeServer.run [`Msg "GARBAGE\r\n"] in

      check_raises
        "Unknown protocol"
        (NatsError (ProtocolError "unknown protocol"))
        (fun () -> ignore @@ Client.connect ~url:(FakeServer.client_url ns) ())
    end;

    "connect: CRLF expected" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG \r\n";
        ]
      in

      check_raises
        "CRLF expected"
        (NatsError (ProtocolError "CRLF expected"))
        (fun () -> ignore @@ Client.connect ~url:(FakeServer.client_url ns) ())
    end;

    (* }}} *)

    (* {{{ The [subscribe] function tests.
       ------------------------------------------------------------------------
    *)

    "subscribe: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      ignore @@ Client.subscribe nc "greet.*"
    end;

    "subscribe: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> ignore @@ Client.subscribe nc "greet.*")
    end;

    (* }}} *)

    (* {{{ The [publish] function tests.
       ------------------------------------------------------------------------
    *)

    "publish: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Client.publish nc "greet.joe" "msg";

      let msg = Subscription.next_msg sub in

      check message "Message received"
        (Message.make
           ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg"
           ())
        msg
    end;

    "publish: with reply" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Client.publish nc "greet.joe" "msg" ~reply:"reply";

      let msg = Subscription.next_msg sub in

      check message "Message received"
        (Message.make
           ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~reply:"reply" ~payload:"msg"
           ())
        msg
    end;

    "publish: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> Client.publish nc "greet.joe" "msg")
    end;

    (* }}} *)

    (* {{{ The I/O loop tests.
       ------------------------------------------------------------------------
    *)

    "I/O loop: lost connection" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Msg "PING\r\n";
          `Read;
        ]
      in

      let nc = Client.connect ~url:(FakeServer.client_url ns) () in

      wait_until_closed nc 10;
      check (option nats_error) "Connection lost"
        (Some ConnectionLost) (Client.last_error nc)
    end;

    "I/O loop: unknown protocol" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Msg "GARBAGE\r\n";
          `Delay 0.1;
        ]
      in

      let nc = Client.connect ~url:(FakeServer.client_url ns) () in

      wait_until_closed nc 10;
      check (option nats_error) "Unknown protocol"
        (Some (ProtocolError "unknown protocol")) (Client.last_error nc)
    end;

    "I/O loop: CRLF expected" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Msg "PING \r\n";
          `Delay 0.1;
        ]
      in

      let nc = Client.connect ~url:(FakeServer.client_url ns) () in

      wait_until_closed nc 10;
      check (option nats_error) "CRLF expected"
        (Some (ProtocolError "CRLF expected")) (Client.last_error nc)
    end;

    (* }}} *)

    (* {{{ The message handling loop tests.
       ------------------------------------------------------------------------
    *)

    "message loop: callback" -: begin fun () ->
      let called = ref false in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" ~callback:(fun _ -> called := true) in

      Client.publish nc "greet.joe" "msg";
      Subscription.drain sub;

      check bool "Callback called" true !called
    end;

    "message loop: callback failure" -: begin fun () ->
      let error = ref None in

      let nc = Client.connect
          ~url:(Server.client_url ns)
          ~error_cb:(fun _ err -> error := Some err)
          ()
      in
      let sub = Client.subscribe nc "greet.*" ~callback:(fun _ -> failwith "error") in

      Client.publish nc "greet.joe" "msg";
      Subscription.drain sub;

      check (option nats_error) "Callback failed"
        (Some (MessageCallbackError (Failure "error"))) !error
    end;

    (* }}} *)

    (* {{{ The [request] function tests.
       ------------------------------------------------------------------------
    *)

    "request: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      ignore @@ Client.subscribe nc "greet.*"
        ~callback:(fun msg ->
            Client.publish nc
              (Option.get msg.reply)
              (Printf.sprintf "response to %s" msg.payload));

      let resp = Client.request nc "greet.joe" "msg" in

      check string "Response received" "response to msg" resp.payload;
    end;

    "request: timeout" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->

      check_raises
        "Timeout"
        (NatsError Timeout)
        (fun () -> ignore @@ Client.request nc "greet.joe" "msg" ~timeout:0.1)
    end;

    "request: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> ignore @@ Client.request nc "greet.joe" "msg")
    end;

    "request: lost connection" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      ignore @@ Thread.create
        begin fun () ->
          Thread.delay 0.2;
          Server.stop ns
        end
        ();

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> ignore @@ Client.request nc "greet.joe" "msg")
    end;

    (* }}} *)

    (* {{{ The [request_opt] function tests.
       ------------------------------------------------------------------------
    *)

    "request_opt: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      ignore @@ Client.subscribe nc "greet.*"
        ~callback:(fun msg ->
            Client.publish nc
              (Option.get msg.reply)
              (Printf.sprintf "response to %s" msg.payload));

      let resp = Client.request_opt nc "greet.joe" "msg" in

      match resp with
      | Some resp -> check string "Response received" "response to msg" resp.payload;
      | None      -> failwith "No response received"
    end;

    "request_opt: timeout" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->

      let resp = Client.request_opt nc "greet.joe" "msg" ~timeout:0.1 in

      check (option message) "No response received" None resp
    end;

    "request_opt: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> ignore @@ Client.request_opt nc "greet.joe" "msg")
    end;

    "request_opt: lost connection" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      ignore @@ Thread.create
        begin fun () ->
          Thread.delay 0.2;
          Server.stop ns
        end
        ();

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> ignore @@ Client.request_opt nc "greet.joe" "msg")
    end;

    (* }}} *)

    (* {{{ The [flush] function tests.
       ------------------------------------------------------------------------
    *)

    "flush: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";
      Client.flush nc;

      check int "The messages delivered" 3 (Subscription.pending_msgs sub)
    end;

    "flush: timeout" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Delay 0.2;
        ]
      in
      let nc = Client.connect ~url:(FakeServer.client_url ns) () in

      check_raises
        "Timeout"
        (NatsError Timeout)
        (fun () -> Client.flush nc ~timeout:0.1);

      Client.close nc
    end;

    "flush: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> Client.flush nc)
    end;

    "flush: lost connection" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Delay 0.1;
        ]
      in
      Client.with_client ~url:(FakeServer.client_url ns) @@ fun nc ->

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> Client.flush nc ~timeout:0.2)
    end;

    (* }}} *)

    (* {{{ The [close] function tests.
       ------------------------------------------------------------------------
    *)

    "close: success" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc
    end;

    "close: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> Client.close nc)
    end;

    (* }}} *)

    (* {{{ The [drain] function tests.
       ------------------------------------------------------------------------
    *)

    "drain: success" -: begin fun () ->
      let counter = ref 0 in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      ignore @@ Client.subscribe nc "greet.*"
          ~callback:(fun _ ->
              Thread.delay 0.1;
              incr counter);

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";

      Client.drain nc;

      check int "Published messages handled" 3 !counter
    end;

    "drain: timeout" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Delay 0.2;
        ]
      in
      let nc = Client.connect ~url:(FakeServer.client_url ns) () in

      check_raises
        "Timeout"
        (NatsError Timeout)
        (fun () -> Client.drain nc ~timeout:0.1);
      check bool "Connection closed" true (Client.is_closed nc)
    end;

    "drain: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      Client.close nc;

      check_raises
        "Connection closed"
        (NatsError ConnectionClosed)
        (fun () -> Client.drain nc)
    end;

    "drain: lost connection" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Delay 0.1;
        ]
      in
      Client.with_client ~url:(FakeServer.client_url ns) @@ fun nc ->

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> Client.drain nc ~timeout:0.2);
      check bool "Connection closed" true (Client.is_closed nc)
    end;

    (* }}} *)
  ]
