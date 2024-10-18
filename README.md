# nats.ml

An [OCaml](https://ocaml.org/) client for the [NATS messaging system](https://nats.io).


## Overview

`nats.ml` is an OCaml library designed to interface with the NATS messaging system.  It aims to support different concurrency models in OCaml, including Unix threads, Lwt, Async, and Eio, making it versatile for a variety of use cases.


## Current Status

Note: This library is in active development and is **not yet production-ready**.  The current focus is on developing the [Unix-based client](unix).  Future plans include support for [Lwt](https://github.com/ocsigen/lwt), [Async](https://github.com/janestreet/async), and [Eio](https://github.com/ocaml-multicore/eio).


## License

Distributed under the [ISC License](LICENSE).
