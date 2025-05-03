(lang dune 3.0)
(using menhir 2.1)
(generate_opam_files true)

(name ocaml-func)
(version 0.1.0)
(authors "Georgiy Komarov")
(license LGPL-3.0-or-later)
(source (github nowarp/ocaml-func))
(maintainers "Georgiy Komarov <jubnzv@gmail.com>")

(package
  (name ocaml-func)
  (synopsis "FunC utilities")
  (description "Provides FunC parser and AST")
  (depends
    (ocaml (>= 5.0))
    core
    core_unix
    (ppx_deriving :build)
    zarith
    cmdliner
    (alcotest :with-test)))
