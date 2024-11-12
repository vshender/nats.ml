(** The [Nats_unix] library tests. *)

open Alcotest
open Nats_server

let () =
  try
    Server.with_server @@ fun ns ->
    run ~and_exit:false ~compact:true "NATS Unix client" [
      Test_client.tests ns;
      Test_subscription.tests ns;
    ]
  with Test_error ->
    exit 1
