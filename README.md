# doctor

[![CI](https://github.com/funwithcthulhu/doctor/actions/workflows/ci.yml/badge.svg)](https://github.com/funwithcthulhu/doctor/actions/workflows/ci.yml)
[![opam](https://badgen.net/opam/v/doctor)](https://opam.ocaml.org/packages/doctor/)
[![license](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

`doctor` checks a local OCaml development environment. It reports missing tools,
suspicious opam state, and editor setup issues; it does not modify switches,
shell files, or editor settings.

It currently checks platform details, core tool versions, opam initialization
state, active and available switches, whether the resolved `ocaml` appears to
match the active switch, whether installed switch tools are visible on `PATH`,
selected opam packages, and the VS Code OCaml Platform extension when `code` is
available.

The read-only diagnostic contract is described in
[docs/diagnostic-contract.md](docs/diagnostic-contract.md).

When opam has an active switch but switch tools are missing from the current
shell, `doctor` reports the active switch bin path and suggests reloading the
opam environment. On Windows it prints PowerShell and cmd.exe commands; on
Unix-like systems it uses `eval $(opam env)`.

## Installation

```console
opam update
opam install doctor
```

To build from a checkout:

```console
git clone https://github.com/funwithcthulhu/doctor
cd doctor
opam install . --deps-only --with-test
opam exec -- dune build
```

For local testing through opam, use a path pin:

```console
opam pin add doctor . -y --kind=path
```

## Usage

```console
doctor check
doctor check --json
doctor version
doctor --help
```

`doctor check` prints a text report:

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

Use `--json` when another program needs to read the report:

```console
$ doctor check --json
{
  "summary": {
    "status": "warn",
    "exit_code": 1
  },
  "diagnostics": [
    {
      "name": "command.ocamlformat",
      "status": "warn",
      "message": "ocamlformat not installed",
      "details": ["Suggested fix: opam install ocamlformat"]
    }
  ]
}
```

Diagnostic `name` values are intended to be stable for scripts. Current names:

- `platform.os`
- `command.opam`
- `command.ocaml`
- `command.dune`
- `command.ocaml-lsp-server`
- `command.ocamlformat`
- `opam.initialized`
- `opam.switch.active`
- `opam.switch.list`
- `opam.env.sync`
- `opam.package.dune`
- `opam.package.ocaml-lsp-server`
- `opam.package.ocamlformat`
- `opam.package.utop`
- `opam.packages`
- `editor.vscode.command`
- `editor.vscode.ocaml-platform`
- `editor.vscode.extensions`

`doctor version` prints:

```console
doctor 0.3.0
```

## Exit Codes

- `0`: no warnings or errors
- `1`: one or more warnings, no errors
- `2`: one or more errors
- `3`: unexpected internal failure

## Development

```console
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune runtest
opam exec -- dune exec doctor -- check
```

Tests fake process execution, so they do not depend on the host opam setup, VS Code,
or a particular shell.

Maintainer release notes are in [RELEASE.md](RELEASE.md).
