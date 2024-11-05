let () =
  Alcotest.run ~compact:true "NATS Server" [
    Test_server.tests_start;
    Test_server.tests_stop;
    Test_server.tests_with_server;
    Test_server.tests_is_running;
    Test_server.tests_client_url;
  ]
