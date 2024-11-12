(** Useful utilities. *)

open Nats_unix

(** An auxiliary operator for defining test cases. *)
let (-:) name f = Alcotest.test_case name `Quick f

(** The implementation of [Alcotest.testable] for [Message.t]. *)
let message = Alcotest.testable Message.pp Message.equal

(** The implementation of [Alcotest.testable] for [nats_error]. *)
let nats_error = Alcotest.testable Errors.pp Errors.equal

(** [wait_until_closed c n] waits until the client [c] is closed or until [n]
    attempts have been made.  The function checks if the client is closed and,
    if not, delays for a short period before retrying.

    Raises [Failure] if [n] attempts have been made but the client is still not
    closed. *)
let rec wait_until_closed c n =
  if Client.is_closed c then
    ()
  else if n = 0 then
    failwith "not closed"
  else begin
    Thread.delay 0.05;
    wait_until_closed c (pred n)
  end

(** [gen_msg_callback ()] generates a pair of functions for handling messages.

    The first function adds a message to an internal list, while the second
    function retrieves all messages in the order they were added. *)
let gen_msg_callback () =
  let messages = ref [] in
  let add_message msg = messages := msg :: !messages
  and get_messages () = List.rev !messages
  in add_message, get_messages

(** A sample [INFO] message. *)
let info_msg = {|INFO {"server_id":"NBHIR4WAQRLKE45Y6BQOMR4QKHGFUU5GO7KURXHOU3GOQERV3HW2HMSJ","server_name":"NBHIR4WAQRLKE45Y6BQOMR4QKHGFUU5GO7KURXHOU3GOQERV3HW2HMSJ","version":"2.10.20","proto":1,"git_commit":"7140387","go":"go1.22.6","host":"0.0.0.0","port":4222,"headers":true,"max_payload":1048576,"client_id":8,"client_ip":"172.17.0.1","xkey":"XBIFOYX5R6337VWAH62B2SZLLMRCJBRSBKT63VFNTBDXN5NG25HG4PV7"}|} ^ "\r\n"
