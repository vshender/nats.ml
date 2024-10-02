# nats-unix

An [OCaml](https://ocaml.org/) client for [NATS messaging system](https://nats.io) based on the standard `Unix` module.

## Features

- **Standard Unix Module**: Built using OCaml's standard `Unix` module, making it suitable for projects that don't use asynchronous programming libraries like [Lwt](https://github.com/ocsigen/lwt) or [Async](https://github.com/janestreet/async).
- **Separate I/O Thread**: Utilizes a dedicated thread for all network I/O operations.  This design ensures that the main thread remains unblocked and responsive, simplifying the development of applications that use the client.

## Limitations

- **Not Thread-Safe**: The client is **not thread-safe**.  Internal synchronization is used to manage state between the I/O thread and the main thread, but external synchronization is required if the client is accessed from multiple threads.  Users should ensure that all interactions with the client occur from the same thread or implement appropriate locking mechanisms.

## Basic usage

### Connecting to NATS

```ocaml
open Nats_unix

let () =
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in
  (* Use the client for publishing or subscribing *)
  ()
```

### Publishing Messages

```ocaml
open Nats_unix

let () =
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in
  Client.publish nc "greetings" "Hello, NATS!";
  Client.flush nc
```

### Subscribing to Subjects

```ocaml
open Nats_unix

let () =
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in
  let _sub = Client.subscribe nc "greetings"
    ~callback:(fun msg ->
       Printf.printf "Received message: %s\n" msg.Message.payload)
  in
  (* Keep the client running to receive messages *)
  ()
```

### Synchronous Subscriptions

```ocaml
open Nats_unix

let () =
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in
  let sub = Client.subscribe nc "greetings" in
  match Subscription.next_msg sub ~timeout:1.0 with
  | Some response ->
    Printf.printf "Received message: %s\n" response.Message.payload
  | None ->
    Printf.eprintf "Request timed out\n";
  Client.close nc
```

### Request/Reply Pattern

```ocaml
open Nats_unix

let () =
  let nc = Client.connect ~url:"nats://127.0.0.1:4222" () in
  match Client.request nc "time" "What time is it?" ~timeout:1.0 with
  | Some response ->
    Printf.printf "Received response: %s\n" response.Message.payload
  | None ->
    Printf.eprintf "Request timed out\n";
  Client.close nc
```

## Examples

See [examples](../examples/nats_unix) for more usage scenarios.
