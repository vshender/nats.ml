(** The NATS subscription tests. *)

open Alcotest

open Nats_server
open Nats_unix

open Util

let tests ns = "NATS subscription", [
    (* {{{ The [subject] function tests.
       ------------------------------------------------------------------------
    *)

    "subject: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check string "Subject is valid" "greet.*" (Subscription.subject sub)
    end;

    (* }}} *)

    (* {{{ The [group] function tests.
       ------------------------------------------------------------------------
    *)

    "group: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" ~group:"greet" in

      check (option string) "Group is valid" (Some "greet") (Subscription.group sub)
    end;

    "group: no group" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check (option string) "No group" None (Subscription.group sub)
    end;

    (* }}} *)

    (* {{{ The [is_sync] function tests.
       ------------------------------------------------------------------------
    *)

    "is_sync: sync subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check bool "Synchronous susbscription" true (Subscription.is_sync sub)
    end;

    "is_sync: async subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in

      check bool "Asynchronous subscription" false (Subscription.is_sync sub)
    end;

    (* }}} *)

    (* {{{ The [is_closed] function tests.
       ------------------------------------------------------------------------
    *)

    "is_closed: created" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      check bool "Not closed" false (Subscription.is_closed sub)
    end;

    "is_closed: unsubscribed" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in
      Subscription.unsubscribe sub;

      check bool "Closed" true (Subscription.is_closed sub)
    end;

    "is_closed: autounsubscribe" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      Subscription.unsubscribe sub ~max_msgs:2;

      check bool "Not closed" false (Subscription.is_closed sub);

      Client.publish nc "greet.joe" "msg 1";
      Client.flush nc;

      check bool "Not closed" false (Subscription.is_closed sub);

      Client.publish nc "greet.joe" "msg 2";
      Client.flush nc;

      check bool "Closed" true (Subscription.is_closed sub)
    end;

    (* }}} *)

    (* {{{ The [delivered] function tests.
       ------------------------------------------------------------------------
    *)

    "delivered: no messages" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      check int "No messages" 0 (Subscription.delivered sub)
    end;

    "delivered: messages received" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.joe" "msg 3";
      Client.publish nc "greet.ted" "msg 4";
      Client.flush nc;

      check int "Messages received" 2 (Subscription.delivered sub)
    end;

    (* }}} *)

    (* {{{ The [pending_msgs] function tests.
       ------------------------------------------------------------------------
    *)

    "pending_msgs: no messages" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      check int "No messages" 0 (Subscription.pending_msgs sub)
    end;

    "pending_msgs: messages received" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.joe" "msg 3";
      Client.publish nc "greet.ted" "msg 4";
      Client.flush nc;

      check int "Two pending messages" 2 (Subscription.pending_msgs sub);

      ignore @@ Subscription.next_msg sub;

      check int "One pending messages" 1 (Subscription.pending_msgs sub);

      ignore @@ Subscription.next_msg sub;

      check int "No pending messages" 0 (Subscription.pending_msgs sub);
    end;

    (* }}} *)

    (* {{{ The [max_msgs] function tests.
       ------------------------------------------------------------------------
    *)

    "max_msgs: created" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check (option int) "No max_msgs" None (Subscription.max_msgs sub)
    end;

    "max_msgs: unsubscribed" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Subscription.unsubscribe sub;

      check (option int) "No max_msgs" None (Subscription.max_msgs sub)
    end;

    "max_msgs: autounsubscribe" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Subscription.unsubscribe sub ~max_msgs:3;

      check (option int) "max_msgs is set" (Some 3) (Subscription.max_msgs sub)
    end;

    (* }}} *)

    (* {{{ The [next_msg] function tests.
       ------------------------------------------------------------------------
    *)

    "next_msg: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.joe" "msg 3";
      Client.publish nc "greet.ted" "msg 4";

      let msg1 = Subscription.next_msg sub
      and msg2 = Subscription.next_msg sub in

      check (list message) "Messages received"
        [
          (Message.make
             ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
             ());
          (Message.make
             ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 3"
             ());
        ]
        [msg1; msg2]
    end;

    "next_msg: wildcard subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "farewell.pam" "msg 1";
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "farewell.ted" "msg 4";

      let msg1 = Subscription.next_msg sub
      and msg2 = Subscription.next_msg sub in

      check (list message) "Messages received"
        [
          (Message.make
             ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
             ());
          (Message.make
             ~subject:"greet.bob" ~sid:(Subscription.sid sub) ~payload:"msg 3"
             ());
        ]
        [msg1; msg2]
    end;

    "next_msg: timeout" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check_raises
        "No messages"
        (NatsError Timeout)
        (fun () -> ignore @@ Subscription.next_msg sub ~timeout:0.1)
    end;

    "next_msg: async subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in

      check_raises
        "Async subscription"
        (NatsError SyncSubRequired)
        (fun () -> ignore @@ Subscription.next_msg sub)
    end;

    "next_msg: closed subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in
      Subscription.unsubscribe sub;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.next_msg sub)
    end;

    "next_msg: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" in
      Client.close nc;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.next_msg sub)
    end;

    "next_msg: lost connection" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      ignore @@ Thread.create
        begin fun () ->
          Thread.delay 0.2;
          Server.stop ns
        end
        ();

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> ignore @@ Subscription.next_msg sub);
    end;

    (* }}} *)

    (* {{{ The [next_msg_opt] function tests.
       ------------------------------------------------------------------------
    *)

    "next_msg_opt: success" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.joe" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.joe" "msg 3";
      Client.publish nc "greet.ted" "msg 4";

      let msg1 = Subscription.next_msg_opt sub
      and msg2 = Subscription.next_msg_opt sub in

      check (list @@ option message) "Messages received"
        [
          Some (Message.make
                  ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
                  ());
          Some (Message.make
                  ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 3"
                  ());
        ]
        [msg1; msg2]
    end;

    "next_msg_opt: wildcard subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "farewell.pam" "msg 1";
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "farewell.ted" "msg 4";

      let msg1 = Subscription.next_msg_opt sub
      and msg2 = Subscription.next_msg_opt sub in

      check (list @@ option message) "Messages received"
        [
          Some (Message.make
                  ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
                  ());
          Some (Message.make
                  ~subject:"greet.bob" ~sid:(Subscription.sid sub) ~payload:"msg 3"
                  ());
        ]
        [msg1; msg2]
    end;

    "next_msg_opt: timeout" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      let msg = Subscription.next_msg_opt sub ~timeout:0.1 in
      check (option message) "No message" None msg
    end;

    "next_msg_opt: async subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in

      check_raises
        "Async subscription"
        (NatsError SyncSubRequired)
        (fun () -> ignore @@ Subscription.next_msg_opt sub)
    end;

    "next_msg_opt: closed subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in
      Subscription.unsubscribe sub;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.next_msg_opt sub)
    end;

    "next_msg_opt: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" in
      Client.close nc;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.next_msg_opt sub)
    end;

    "next_msg_opt: lost connection" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      ignore @@ Thread.create
        begin fun () ->
          Thread.delay 0.2;
          Server.stop ns
        end
        ();

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> ignore @@ Subscription.next_msg_opt sub);
    end;

    (* }}} *)

    (* {{{ Asynchronous subscription tests.
       ------------------------------------------------------------------------
    *)

    "async sub: handle messages" -: begin fun () ->
      let add_message, get_messages = gen_msg_callback () in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.joe" ~callback:add_message in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.joe" "msg 3";
      Client.publish nc "greet.ted" "msg 4";
      Client.drain nc;

      check (list message) "Messages received"
        [
          Message.make
            ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
            ();
          Message.make
            ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 3"
            ();
        ]
        (get_messages ())
    end;

    "async sub: wildcard subscription" -: begin fun () ->
      let add_message, get_messages = gen_msg_callback () in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" ~callback:add_message in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "farewell.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "farewell.ted" "msg 4";
      Client.drain nc;

      check (list message) "Messages received"
        [
          Message.make
            ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
            ();
          Message.make
            ~subject:"greet.bob" ~sid:(Subscription.sid sub) ~payload:"msg 3"
            ();
        ]
        (get_messages ())
    end;

    (* }}} *)

    (* {{{ Synchronous subscription unsubscribe tests.
       ------------------------------------------------------------------------
    *)

    "sync sub unsub: get buffered messages" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.flush nc;
      Subscription.unsubscribe sub;
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "greet.ted" "msg 4";

      let msg1 = Subscription.next_msg sub
      and msg2 = Subscription.next_msg sub in
      check (list message) "Messages received"
        [
          (Message.make
             ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
             ());
          (Message.make
             ~subject:"greet.pam" ~sid:(Subscription.sid sub) ~payload:"msg 2"
             ());
        ]
        [msg1; msg2];
      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.next_msg sub)
    end;

    "sync sub unsub: autounsubscribe" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in
      Subscription.unsubscribe sub ~max_msgs:2;

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "greet.ted" "msg 4";

      let msg1 = Subscription.next_msg sub
      and msg2 = Subscription.next_msg sub in
      check (list message) "Messages received"
        [
          (Message.make
             ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
             ());
          (Message.make
             ~subject:"greet.pam" ~sid:(Subscription.sid sub) ~payload:"msg 2"
             ());
        ]
        [msg1; msg2];
      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.next_msg sub)
    end;

    "sync sub unsub: closed subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" in

      Subscription.unsubscribe sub;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.unsubscribe sub)
    end;

    "sync sub unsub: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" in

      Client.close nc;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.unsubscribe sub)
    end;

    (* }}} *)

    (* {{{ Asynchronous subscription unsubscribe tests.
       ------------------------------------------------------------------------
    *)

    "async sub unsub: unsubscribe" -: begin fun () ->
      let add_message, get_messages = gen_msg_callback () in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" ~callback:add_message in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.flush nc;
      Subscription.unsubscribe sub;
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "greet.ted" "msg 4";
      Client.drain nc;

      check (list message) "Messages received"
        [
          Message.make
            ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
            ();
          Message.make
            ~subject:"greet.pam" ~sid:(Subscription.sid sub) ~payload:"msg 2"
            ();
        ]
        (get_messages ())
    end;

    "async sub unsub: autounsubscribe" -: begin fun () ->
      let add_message, get_messages = gen_msg_callback () in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" ~callback:add_message in
      Subscription.unsubscribe sub ~max_msgs:2;

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "greet.ted" "msg 4";
      Client.drain nc;

      check (list message) "Messages received"
        [
          Message.make
            ~subject:"greet.joe" ~sid:(Subscription.sid sub) ~payload:"msg 1"
            ();
          Message.make
            ~subject:"greet.pam" ~sid:(Subscription.sid sub) ~payload:"msg 2"
            ();
        ]
        (get_messages ())
    end;

    "async sub unsub: closed subscription" -: begin fun () ->
      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in

      Subscription.unsubscribe sub;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.unsubscribe sub)
    end;

    "async sub unsub: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in

      Client.close nc;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> ignore @@ Subscription.unsubscribe sub)
    end;

    (* }}} *)

    (* {{{ The [drain] function tests.
       ------------------------------------------------------------------------
    *)

    "sub drain: success" -: begin fun () ->
      let counter = ref 0 in

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*"
          ~callback:(fun _ ->
              Thread.delay 0.1;
              incr counter)
      in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";

      Subscription.drain sub;

      check int "Published messages handled" 3 !counter
    end;

    "sub drain: timeout" -: begin fun () ->
      let counter = ref 0 in

      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*"
          ~callback:(fun _ ->
              Thread.delay 0.1;
              incr counter)
      in

      Client.publish nc "greet.joe" "msg 1";
      Client.publish nc "greet.pam" "msg 2";
      Client.publish nc "greet.bob" "msg 3";
      Client.publish nc "greet.ted" "msg 4";

      check_raises
        "Timeout"
        (NatsError Timeout)
        (fun () -> Subscription.drain sub ~timeout:0.21);

      check int "Two messages handled" 2 !counter;

      Client.close nc;

      check int "One more message handled" 3 !counter
    end;

    "sub drain: closed connection" -: begin fun () ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in
      Client.close nc;

      check_raises
        "Subscription closed"
        (NatsError SubscriptionClosed)
        (fun () -> Subscription.drain sub)
    end;

    "sub drain: connection lost" -: begin fun () ->
      let ns = FakeServer.run [
          `Msg info_msg;
          `Read;
          `Msg "PONG\r\n";
          `Delay 0.2;
        ]
      in
      Client.with_client ~url:(FakeServer.client_url ns) @@ fun nc ->
      let sub = Client.subscribe nc "greet.*" ~callback:(Fun.const ()) in

      check_raises
        "Connection lost"
        (NatsError ConnectionLost)
        (fun () -> Subscription.drain sub)
    end;

    (* }}} *)

    (* {{{ Multiple subscriptions tests.
       ------------------------------------------------------------------------
    *)

    "multiple subscriptions" -: begin fun () ->
      let counter = ref 0 in

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let _ = Client.subscribe nc "greet.*" ~callback:(fun _ -> incr counter)
      and _ = Client.subscribe nc "greet.*" ~callback:(fun _ -> incr counter)
      in

      [
        "joe"; "pam"; "bob"; "ted"; "ada";
        "ian"; "mia"; "leo"; "zoe"; "max";
      ] |> List.iteri
        (fun i name ->
           Client.publish
             nc (Printf.sprintf "greet.%s" name) (Printf.sprintf "msg %d" i));
      Client.drain nc;

      check int "Each message was processed twice" 20 !counter
    end;

    "multiple queue subscriptions" -: begin fun () ->
      let counter = ref 0 in

      Client.with_client ~url:(Server.client_url ns) @@ fun nc ->
      let _ = Client.subscribe nc "greet.*" ~group:"test" ~callback:(fun _ -> incr counter)
      and _ = Client.subscribe nc "greet.*" ~group:"test" ~callback:(fun _ -> incr counter)
      in

      [
        "joe"; "pam"; "bob"; "ted"; "ada";
        "ian"; "mia"; "leo"; "zoe"; "max";
      ] |> List.iteri
        (fun i name ->
           Client.publish
             nc (Printf.sprintf "greet.%s" name) (Printf.sprintf "msg %d" i));
      Client.drain nc;

      check int "Each message was processed once" 10 !counter
    end;

    (* }}} *)
  ]
