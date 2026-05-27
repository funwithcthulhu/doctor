(lang dune 3.11)

(name doctor)
(version 0.3.0)

(source
 (github funwithcthulhu/doctor))

(authors "Thomas B.")
(license MIT)

(maintainers "Thomas B. <funwithcthulhu29@gmail.com>")

(package
 (name doctor)
 (synopsis "Diagnostic CLI for OCaml development environments")
 (description
  "doctor checks common OCaml development environment problems, including opam setup, active switches, dune, ocaml-lsp-server, ocamlformat, PATH issues, and editor integration hints.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.11))
  (cmdliner (>= 1.1.0))))
