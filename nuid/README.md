# NUID

A highly performant unique identifier generator.

## Installation

Use the `opam` command:
```
$ opam install nuid
```

## Basic Usage

```ocaml
(* Utilize the global locked generator. *)
# Nuid.next () ;;
- : string = "TPxizw6qtJTWCsj6NQRTZt"

(* Create a NUID generation state instance. *)
# let nst = Nuid.State.create () ;;
val nst : Nuid.State.t = <abstr>

(* Generate unique identifiers. *)
# Nuid.State.next nst ;;
- : string = "0fmL4InTGejJB8FVu1lU2v"
# Nuid.State.next nst ;;
- : string = "0fmL4InTGejJB8FVu1lU6G"
utop # Nuid.State.next nst ;;
- : string = "0fmL4InTGejJB8FVu1lU9b"
```

## Performance

NUID needs to be very fast to generate and be truly unique, all while being
entropy pool friendly.  NUID uses 12 bytes of crypto generated data (entropy
draining), and 10 bytes of pseudo-random sequential data that increments with
a pseudo-random increment.

Total length of a NUID string is 22 bytes of base 62 ASCII text, so there are
$62^{22}$ or $2707803647802660400290261537185326956544$ possibilities.

NUID can generate identifiers as fast as 40ns, or ~25 million per second.
There is an associated [benchmark](../nuid_benchmark) you can use to test
performance on your own hardware.
