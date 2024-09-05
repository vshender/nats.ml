(lang dune 3.4)

(name nats)

(source
 (github vshender/nats.ml))
(homepage "https://github.com/vshender/nats.ml")
(bug_reports "https://github.com/vshender/nats.ml/issues")

(authors "Vadim Shender <vadim@shender.org>")
(maintainers "Vadim Shender <vadim@shender.org>")

(license "ISC")

(generate_opam_files true)

(package
 (name nats)
 (synopsis
  "OCaml client library for NATS, the cloud native messaging system")
 (depends
  (ocaml (>= 4.14.0))
  (angstrom (>= 0.16.0))
  (faraday (>= 0.8.2))
  (ppx_deriving (>= 6.0.2))
  (ppx_deriving_yojson (>= 3.8.0))
  (alcotest (and (>= 1.7.0) :with-test))))

(package
 (name nuid)
 (synopsis
  "A highly performant unique identifier generator")
 (depends
  (ocaml (>= 4.14.0))
  (alcotest (and (>= 1.7.0) :with-test))
  (benchmark (and (>= 1.6) :with-test))))
