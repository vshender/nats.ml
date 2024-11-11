(** The module for managing a fake NATS server for edge case testing
    purposes. *)

open Unix

type t = {
  port : int;
}

let run ?port msgs =
  let port = Option.value port ~default:0 in
  let addr = ADDR_INET (inet_addr_of_string "127.0.0.1", port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock addr;

  let port = match getsockname sock with
    | ADDR_INET (_, port) -> port
    | ADDR_UNIX _         -> assert false
  in

  listen sock 1;

  ignore @@ Thread.create
    begin fun () ->
      let sock, _ = accept ~cloexec:true sock in
      let buf = Bytes.create 0x1000 in
      msgs |> List.iter
        begin fun msg ->
          match msg with
          | `Msg msg -> ignore @@ write_substring sock msg 0 (String.length msg)
          | `Read    -> ignore @@ read sock buf 0 (Bytes.length buf);
          | `Delay n -> Thread.delay n
        end;
      shutdown sock SHUTDOWN_ALL;
      close sock
    end
    ();

  { port = port }

let client_url t =
  Printf.sprintf "nats://127.0.0.1:%d" t.port
