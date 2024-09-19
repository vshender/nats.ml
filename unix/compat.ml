(** Missing functionality for older versions of OCaml. *)

module Unix = struct
  include Unix

#if OCAML_VERSION < (5, 2, 0)
  let write_bigarray fd a off len =
    let str = Bigstringaf.substring a ~off ~len in
    Unix.write_substring fd str 0 len
#endif
end
