open Unix

open Alcotest

open Nats_server
open Nats.Parse
open Nats.Protocol

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** The implementation of [Alcotest.testable] for [ServerMessage.t]. *)
let server_message = testable ServerMessage.pp ServerMessage.equal

(** Exception raised when a timeout occurs during a socket operation. *)
exception Timed_out

(** [read ?timeout fd buf pos len] reads data from the descriptor [fd] into the
    buffer [buf] starting at position [pos] and reading up to [len] bytes.
    Returns the number of bytes actually read. *)
let read ?(timeout = -1.) sock buf pos len =
  (* Wait for the socket to be readable. *)
  let (readable, _, _) = select [sock] [] [] timeout in
  if readable = [] then
    raise Timed_out;

  Unix.read sock buf pos len

let tests = "NATS fake server", [
    "test fake server" -: begin fun () ->
      let buf = Bytes.create 0x100
      and reader = Reader.create () in

      let ns = FakeServer.run [
          `Msg "PING\r\n";
          `Delay 0.5;
          `Msg "PONG\r\n";
          `Read;
          `Msg "MSG subject 1 5\r\nhello\r\n";
        ]
      in

      let url = Uri.of_string @@ FakeServer.client_url ns in
      let addr = ADDR_INET (
          inet_addr_of_string @@ Option.get @@ Uri.host url,
          Option.get @@ Uri.port url
        ) in
      let sock = socket PF_INET SOCK_STREAM 0 in
      connect sock addr;

      let n = read sock buf 0 (Bytes.length buf) in
      Reader.feed reader buf 0 n;

      let start = gettimeofday () in
      let n = read sock buf 0 (Bytes.length buf) in
      Reader.feed reader buf 0 n;

      check bool "Delay" true (gettimeofday () -. start > 0.3);

      check_raises
        "Expected timeout"
        Timed_out
        (fun () -> ignore @@ read sock buf 0 (Bytes.length buf) ~timeout:0.2);
      ignore @@ write sock buf 0 1;

      let n = read sock buf 0 (Bytes.length buf) in
      Reader.feed reader buf 0 n;

      [
        ServerMessage.Ping;
        ServerMessage.Pong;
        ServerMessage.Msg (Msg.make ~subject:"subject" ~sid:"1" ~payload:"hello" ());
      ] |> List.iter @@ fun msg_expected ->
      match Reader.next_msg reader with
      | Message msg -> check server_message "Expected message received" msg_expected msg
      | _           -> failwith "Message reading error"
    end;
  ]
