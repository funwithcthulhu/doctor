# Changelog

## 0.3.0 - 2026-05-27

- Improve opam switch environment diagnostics when switch tools are installed
  but missing from `PATH`.
- Add shell-specific opam environment guidance and regression tests for stale
  or malformed opam command output.
- Document the read-only diagnostic contract and stable JSON diagnostic names.
- Add golden text report fixtures for the diagnostic renderer.

## 0.2.0 - 2026-05-21

- Add `doctor check --json` for machine-readable diagnostics.
- Tighten opam environment diagnostic tests and small parser helpers.
- Fix the README license badge URL.

## 0.1.0 - 2026-05-06

- Initial opam release.
- Add `doctor check` with diagnostics for common OCaml, opam, dune, LSP, formatter,
  PATH, and editor setup issues.
- Document the text report and exit codes.
