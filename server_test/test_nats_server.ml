(** The [Nats_server] library tests. *)

let () =
  Alcotest.run ~compact:true "NATS Server" [
    Test_server.tests;
    Test_fake_server.tests;
  ]
