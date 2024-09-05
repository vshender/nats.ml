open Benchmark
open Nuid

let _ =
  let st = State.create () in
  throughput1 5 ~name:"NUID State.next" State.next st

let _ =
  throughput1 5 ~name:"global NUID next" next ()
