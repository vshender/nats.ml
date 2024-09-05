(** Tests for NUID generator. *)

open Alcotest

open Nuid.TestingInterface

let quick_tests = "quick tests", [
    test_case "digits length" `Quick begin fun () ->
      String.length State.digits
      |> check int "digits length matches base module" State.base
    end;
    test_case "total_len" `Quick begin fun () ->
      State.(pre_len + seq_len)
      |> check
        int
        "total length is sum of lengths of prefix and sequential components"
        State.total_len
    end;
    test_case "NUID length" `Quick begin fun () ->
      State.(create () |> next) |> String.length
      |> check int "NUID length is valid" State.total_len
    end;
    test_case "rollover" `Quick begin fun () ->
      let st = State.create () in
      st.seq <- State.max_seq;
      let old_pre = Bytes.sub st.cur 0 State.pre_len in
      let _ = State.next st in
      let cur_pre = Bytes.sub st.cur 0 State.pre_len in
      check (neg bytes) "rollover happened" old_pre cur_pre
    end;
  ]

let slow_tests = "slow tests", [
    test_case "digits" `Slow begin fun () ->
      let exception InvalidDigit in
      let module CSet = Set.Make (Char) in
      let digits = String.fold_left
          (fun s c -> CSet.add c s)
          CSet.empty
          State.digits
      in
      try
        for _ = 1 to 1000 do
          let ns = State.create () in
          for _ = 1 to 1000 do
            let id = State.next ns in
            String.iter
              (fun d -> if not (CSet.mem d digits) then raise InvalidDigit)
              id
          done
        done
      with InvalidDigit ->
        fail "invalid digit"
    end;
    test_case "unique" `Slow begin fun () ->
      let ns = State.create () in
      let n = 10_000_000 in
      let nuids = Hashtbl.create n in
      for _ = 1 to n do
        Hashtbl.add nuids (State.next ns) true
      done;
      check int "NUIDs are unique" n (Hashtbl.length nuids)
    end;
  ]

let () =
  run ~compact:true "NUID" [
    quick_tests;
    slow_tests;
  ]
