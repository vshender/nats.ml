let () =
  Alcotest.run ~compact:true "NATS" [
    Test_headers.tests;
    Test_parse.tests;
    Test_serialize.tests;
  ]
