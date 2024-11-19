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
  "An OCaml library providing core building blocks for creating NATS clients")
 (depends
  (ocaml (>= 4.14.0))
  (angstrom (>= 0.15.0))
  (faraday (>= 0.8.2))
  (ppx_deriving (>= 5.2.1))
  (ppx_deriving_yojson (>= 3.7.0))
  (alcotest (and (>= 1.6.0) :with-test))))

(package
 (name nats-server)
 (synopsis
  "A library for managing NATS server instances for testing purposes")
 (depends
  (ocaml (>= 4.14.0))
  (nats (= :version))
  (nuid (= :version))))

(package
 (name nats-unix)
 (synopsis
  "A Unix library-based client for NATS, the cloud native messaging system")
 (depends
  (ocaml (>= 4.14.0))
  (nats (= :version))
  (nuid (= :version))
  (uri (>= 4.2.0))
  (cppo (>= 1.6.9))
  (alcotest (and (>= 1.6.0) :with-test))))

(package
 (name nats-lwt)
 (synopsis
  "An Lwt-based client for NATS, the cloud native messaging system")
 (depends
  (ocaml (>= 4.14.0))
  (nats (= :version))
  (nuid (= :version))
  (lwt (>= 5.6.1))
  (uri (>= 4.2.0))
  (alcotest (and (>= 1.6.0) :with-tests))))

(package
 (name nuid)
 (synopsis
  "A highly performant unique identifier generator")
 (depends
  (ocaml (>= 4.14.0))
  (alcotest (and (>= 1.6.0) :with-test))
  (benchmark (and (>= 1.6) :with-test))))
