# doctor

[![CI](https://github.com/funwithcthulhu/doctor/actions/workflows/ci.yml/badge.svg)](https://github.com/funwithcthulhu/doctor/actions/workflows/ci.yml)
[![opam](https://img.shields.io/opam/v/doctor.svg)](https://opam.ocaml.org/packages/doctor/)
[![license](https://img.shields.io/github/license/funwithcthulhu/doctor.svg)](LICENSE)

`doctor` is a small diagnostic CLI for OCaml development environments.

Status: early `0.1.0` tool, intended to be useful and conservative rather than
complete.

## What It Checks

- Platform detection for Windows, macOS, Linux, and WSL where reasonably
  detectable.
- Core tool availability and versions for `opam`, `ocaml`, `dune`, OCaml LSP
  (`ocaml-lsp-server` or `ocamllsp`), and `ocamlformat`.
- Whether opam appears initialized.
- Active and available opam switches.
- Heuristic shell environment sync by comparing the resolved `ocaml` command
  with the active opam switch `bin` directory where possible.
- Installed opam packages for `dune`, `ocaml-lsp-server`, `ocamlformat`, and
  optional `utop`.
- VS Code OCaml Platform extension detection when the `code` command is
  available.

## What It Does Not Do

- It does not replace opam, dune, ocaml-lsp, or editor setup docs.
- It does not run `opam init`, `opam switch create`, or `opam install` for you.
- It does not edit shell startup files or modify user shell configuration.
- It does not implement destructive auto-fixes.
- It does not claim to diagnose every possible OCaml setup issue.

## Installation From Source

```console
git clone https://github.com/funwithcthulhu/doctor
cd doctor
opam install . --deps-only --with-test
dune build
dune runtest
```

If your shell has not been synced with the active opam switch, use
`opam exec -- dune build` and `opam exec -- dune runtest`.

## Local Install

```console
opam pin add doctor . -y
```

When testing uncommitted local changes, prefer a path pin so opam reads the
working tree instead of `git+file://...#main`:

```console
opam pin add doctor . -y --kind=path
```

After publication to opam-repository, the expected user flow is:

```console
opam update
opam install doctor
doctor check
```

## Usage

```console
doctor check
doctor version
doctor --help
```

`doctor version` prints:

```console
doctor 0.1.0
```

## Example Output

```console
$ doctor check
OCaml Doctor

[OK] platform detected: macOS
[OK] opam found: 2.2.1
[OK] OCaml found: 5.2.0
[OK] dune found: 3.17.0
[OK] OCaml LSP found: 1.19.0 (ocamllsp)
[WARN] ocamlformat not installed
       Suggested fix: opam install ocamlformat
[OK] active switch: 5.2.0
[WARN] VS Code OCaml Platform extension not detected
       Suggested fix: Install extension ocamllabs.ocaml-platform in VS Code.

Summary: 6 OK, 2 WARN, 0 ERROR
```

## Exit Codes

- `0`: no warnings or errors
- `1`: one or more warnings, no errors
- `2`: one or more errors
- `3`: unexpected internal failure

## Development

Build:

```console
dune build
```

Run tests:

```console
dune runtest
```

Run locally without installing:

```console
dune exec doctor -- check
```

If `dune` is not available on `PATH`, prefix these commands with
`opam exec --`.

Tests use injectable process runners and deterministic fixtures. They should
not require the local machine to have opam configured, VS Code installed, or a
specific shell setup.

## Contribution Ideas

- Improve shell detection for PowerShell, cmd.exe, MSYS2, and Cygwin.
- Add JSON output for editor integrations and issue templates.
- Add more editor checks without making VS Code mandatory.
- Improve opam switch environment explanations on Windows.
- Add targeted diagnostics for common dune and LSP project-layout problems.

## Release Process

Maintainer release steps are documented in [RELEASE.md](RELEASE.md).
