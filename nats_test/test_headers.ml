(* NATS message headers tests. *)

open Alcotest

open Nats.Protocol.Headers

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** [hdrs_to_list h] converts the given headers [h] to a list of pairs. *)
let hdrs_to_list h = fold (fun k v accu -> (k, v) :: accu) h [] |> List.rev

let tests = "headers", [
    (* {{{ The [version] function tests.
       ------------------------------------------------------------------------
    *)

    "version: 1.0" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one")] () in
      version hdrs
      |> check (pair int int) "version is 1.0" (1, 0)
    end;

    "version: 2.0" -: begin fun () ->
      let hdrs = make ~version:(2, 0) ~headers:[("A", "one")] () in
      version hdrs
      |> check (pair int int) "version is 2.0" (2, 0)
    end;

    (* }}} *)

    (* {{{ The [get] function tests.
       ------------------------------------------------------------------------
    *)

    "get: no value" -: begin fun () ->
      let hdrs = make ~headers:[("B", "two")] () in
      get "A" hdrs
      |> check (option string) "no header value" None
    end;

    "get: single value" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one"); ("B", "two")] () in
      get "A" hdrs
      |> check (option string) "matching header value" (Some "one")
    end;

    "get: multiple values" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one"); ("B", "two"); ("A", "three")] () in
      get "A" hdrs
      |> check (option string) "first matching header value" (Some "one")
    end;

    (* }}} *)

    (* {{{ The [values] function tests.
       ------------------------------------------------------------------------
    *)

    "values: no value" -: begin fun () ->
      let hdrs = make ~headers:[("B", "two")] () in
      values "A" hdrs
      |> check (list string) "no header values" []
    end;

    "values: single value" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one"); ("B", "two")] () in
      values "A" hdrs
      |> check (list string) "matching header value" ["one"]
    end;

    "values: multiple values" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one"); ("B", "two"); ("A", "three")] () in
      values "A" hdrs
      |> check (list string) "all matching header values" ["one"; "three"]
    end;

    (* }}} *)

    (* {{{ The [fold] function tests.
       ------------------------------------------------------------------------
    *)

    "fold: empty headers" -: begin fun () ->
      let hdrs = make ~headers:[] () in
      hdrs_to_list hdrs
      |> check (list (pair string string)) "empty headers" []
    end;

    "fold: single header" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one")] () in
      hdrs_to_list hdrs
      |> check (list (pair string string)) "single header" [("A", "one")]
    end;

    "fold: multiple headers" -: begin fun () ->
      let hdrs = make ~headers:[("A", "one"); ("B", "two"); ("A", "three")] () in
      hdrs_to_list hdrs
      |> check (list (pair string string)) "multiple headers" [("A", "one"); ("B", "two"); ("A", "three")]
    end;

    (* }}} *)
  ]
