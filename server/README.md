# nats-server

An [OCaml](https://ocaml.org/) library providing utilities for managing local instances of the [NATS](https://nats.io) server for testing purposes.  This library is used for clients integration tests.

## Usage example

```ocaml
open Nats_server
open Nats_unix

let () =
  Server.with_server
    begin fun ns ->
      let nc = Client.connect () ~url:(Server.client_url ns) in
      let sub = Client.subscribe nc "subject" in
      Client.publish nc "subject" "hello";
      let msg = Subscription.next_msg sub in
      Printf.printf "Message received: %s\n%!" msg.payload;
      Client.close nc
    end
```
