(** Missing functionality for older versions of OCaml. *)

module Mutex = struct
  include Mutex

#if OCAML_VERSION < (5, 1, 0)
  (* private re-export *)
  external reraise : exn -> 'a = "%reraise"

  (* cannot inline, otherwise flambda might move code around. *)
  let[@inline never] protect m f =
    lock m;
    match f() with
    | x ->
      unlock m; x
    | exception e ->
      (* NOTE: [unlock] does not poll for asynchronous exceptions *)
      unlock m;
      reraise e
#endif
end

module Unix = struct
  include Unix

#if OCAML_VERSION < (5, 2, 0)
  let write_bigarray fd a off len =
    let str = Bigstringaf.substring a ~off ~len in
    Unix.write_substring fd str 0 len
#endif
end
