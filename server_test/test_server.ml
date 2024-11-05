(** Tests for NATS server module. *)

open Alcotest

open Nats_server
open Nats_unix

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** [is_running pid] is [true] if the process with the given [pid] is running,
    and [false] otherwise. *)
let is_running pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error(ESRCH, _, _) ->
    false

let tests_start = "start server tests", [
    "Start a server" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      begin match Server.pid ns with
        | Some pid -> check bool "NATS server is running" true (is_running pid)
        | None     -> failwith "Server.pid is broken"
      end;
      Server.stop ns
    end;
    "Start a running server" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      Fun.protect
        ~finally:(fun () -> Server.stop ns)
        begin fun () ->
          check_raises
            "Fail to start a running server"
            (Failure "NATS server is already running")
            (fun () -> Server.start ns)
        end
    end;
  ]

let tests_stop = "stop server tests", [
    "Stop a server" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      let pid = Server.pid ns in
      Server.stop ns;
      match pid with
      | Some pid -> check bool "NATS server is stopped" false (is_running pid)
      | None     -> failwith "Server.pid is broken"
    end;
    "Stop a not started server" -: begin fun () ->
      let ns = Server.create () in
      check_raises
        "Fail to stop a not running server"
        (Failure "NATS server is not running")
        (fun () -> Server.stop ns)
    end;
    "Stop a stopped server" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      Server.stop ns;
      check_raises
        "Fail to stop a stopped server"
        (Failure "NATS server is not running")
        (fun () -> Server.stop ns)
    end;
  ]

let tests_with_server = "with_server tests", [
    "Start a server using with_server" -: begin fun () ->
      Server.with_server
        begin fun ns ->
          match Server.pid ns with
          | Some pid -> check bool "NATS server is running" true (is_running pid)
          | None     -> failwith "Server.pid is broken"
        end
    end;
    "Check a server after with_server" -: begin fun () ->
      let pid = Server.with_server (fun ns -> Server.pid ns) in
      match pid with
      | Some pid -> check bool "NATS server is stopped" false (is_running pid)
      | None     -> failwith "Server.pid is broken"
    end;
  ]

let tests_is_running = "is_running tests", [
    "Check is_running before start" -: begin fun () ->
      let ns = Server.create () in
      check bool "NATS server is not running" false (Server.is_running ns)
    end;
    "Check is_running after start" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      check bool "NATS server is running" true (Server.is_running ns)
    end;
    "Check is_running after stop" -: begin fun () ->
      let ns = Server.create () in
      Server.start ns;
      Server.stop ns;
      check bool "NATS server is not running" false (Server.is_running ns)
    end;
  ]

let tests_client_url = "client_url tests", [
    "Use the client URL to connect" -: begin fun () ->
      Server.with_server @@ fun ns ->
      let nc = Client.connect ~url:(Server.client_url ns) () in
      let sub = Client.subscribe nc "test-subject" in
      Client.publish nc "test-subject" "test-message";
      let msg = Subscription.next_msg sub in
      check string "Received published message" "test-message" msg.payload;
      Client.close nc
    end;
  ]
