(lang dune 3.11)

(name ocaml-doctor)
(version 0.1.0)

(source
 (github funwithcthulhu/ocaml-doctor))

(authors "Thomas B.")
(license MIT)
(maintainers "Thomas B. <replace-with-email@example.com>")

(package
 (name ocaml-doctor)
 (synopsis "Diagnostic CLI for OCaml development environments")
 (description "ocaml-doctor checks common OCaml development environment problems, including opam setup, active switches, dune, ocaml-lsp-server, ocamlformat, PATH issues, and editor integration hints.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.11))
  (cmdliner (>= 1.1.0))))
