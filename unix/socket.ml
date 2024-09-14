(** Utility functions for working with sockets in OCaml, allowing connection
    and reading operations with support for specifying timeouts.
*)

open Unix

exception Timed_out

let connect ?(timeout = -1.) sock addr =
  set_nonblock sock;
  Fun.protect
    ~finally:begin fun () ->
      clear_nonblock sock
    end
    begin fun () ->
      try
        (* Attempt to connect. *)
        Unix.connect sock addr

      with
      | Unix_error (EINPROGRESS, _, _) ->
        (* The connection is still in progress, wait for the socket to be
           writeable. *)
        let (_, writeable, _) = select [] [sock] [] timeout in
        if writeable = [] then
          raise Timed_out;

        (* Check for connection errors. *)
        match Unix.getsockopt_error sock with
        | Some err -> raise (Unix_error (err, "connect", ""))
        | None     -> ()
    end

let read ?(timeout = -1.) sock buf pos len =
  (* Wait for the socket to be readable. *)
  let (readable, _, _) = select [sock] [] [] timeout in
  if readable = [] then
    raise Timed_out;

  Unix.read sock buf pos len
