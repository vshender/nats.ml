(** Tests for NATS message headers. *)

open Alcotest

open Nats.Protocol.Headers

(** An auxiliary operator for defining test cases. *)
let (-:) name f = test_case name `Quick f

(** Tests for the `version` function. *)
let version_tests = "version", [
  "1.0" -: begin fun () ->
    make ~headers:[("A", "one")] () |> version
    |> check (pair int int) "version is 1.0" (1, 0)
  end;
  "2.0" -: begin fun () ->
    make ~version:(2, 0) ~headers:[("A", "one")] () |> version
    |> check (pair int int) "version is 2.0" (2, 0)
  end;
]

(** Tests for the `get` function. *)
let get_tests = "get", [
  "no value" -: begin fun () ->
    make ~headers:[("B", "two")] () |> get "A"
    |> check (option string) "no header value" None
  end;
  "single value" -: begin fun () ->
    make ~headers:[("A", "one"); ("B", "two")] () |> get "A"
    |> check (option string) "matching header value" (Some "one")
  end;
  "multiple values" -: begin fun () ->
    make ~headers:[("A", "one"); ("B", "two"); ("A", "three")] () |> get "A"
    |> check (option string) "first matching header value" (Some "one")
  end;
]

(** Tests for the `values` function. *)
let values_tests = "values", [
  "no value" -: begin fun () ->
    make ~headers:[("B", "two")] () |> values "A"
    |> check (list string) "no header values" []
  end;
  "single value" -: begin fun () ->
    make ~headers:[("A", "one"); ("B", "two")] () |> values "A"
    |> check (list string) "matching header value" ["one"]
  end;
  "multiple values" -: begin fun () ->
    make ~headers:[("A", "one"); ("B", "two"); ("A", "three")] () |> values "A"
    |> check (list string) "all matching header values" ["one"; "three"]
  end;
]

(** Tests for the `fold` function. *)
let fold_tests =
  let to_list h = fold (fun k v accu -> (k, v) :: accu) h [] |> List.rev in
  "fold", [
    "empty headers" -: begin fun () ->
      make ~headers:[] () |> to_list
      |> check (list (pair string string)) "empty headers" []
    end;
    "single header" -: begin fun () ->
      make ~headers:[("A", "one")] () |> to_list
      |> check (list (pair string string)) "single header" [("A", "one")]
    end;
    "multiple headers" -: begin fun () ->
      make ~headers:[("A", "one"); ("B", "two"); ("A", "three")] () |> to_list
      |> check (list (pair string string)) "multiple headers" [("A", "one"); ("B", "two"); ("A", "three")]
    end
  ]

let () =
  run ~compact:true "Headers" [
    version_tests;
    get_tests;
    values_tests;
    fold_tests;
  ]
