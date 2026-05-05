# ocaml-doctor

`ocaml-doctor` is a small diagnostic CLI for OCaml development environments.

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
git clone https://github.com/funwithcthulhu/ocaml-doctor
cd ocaml-doctor
opam install . --deps-only --with-test
dune build
dune runtest
```

If your shell has not been synced with the active opam switch, use
`opam exec -- dune build` and `opam exec -- dune runtest`.

## Local Install

```console
opam pin add ocaml-doctor . -y
```

Before the repository has its first commit, opam may try to fetch
`git+file://...#main` and fail. For pre-commit local testing only, use:

```console
opam pin add ocaml-doctor . -y --kind=path
```

After publication to opam-repository, the expected user flow is:

```console
opam update
opam install ocaml-doctor
ocaml-doctor check
```

## Usage

```console
ocaml-doctor check
ocaml-doctor version
ocaml-doctor --help
```

`ocaml-doctor version` prints:

```console
ocaml-doctor 0.1.0
```

## Example Output

```console
$ ocaml-doctor check
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
dune exec ocaml-doctor -- check
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

## Release Checklist

```console
# 1. Build and test.
opam install . --deps-only --with-test
dune build
dune runtest
opam lint

# 2. Test local install.
opam pin add ocaml-doctor . -y
ocaml-doctor check
ocaml-doctor version
opam remove ocaml-doctor

# 3. Commit.
git status
git add .
git commit -m "Prepare 0.1.0 release"

# 4. Tag.
git tag -a 0.1.0 -m "Release 0.1.0"
git push origin main
git push origin 0.1.0

# 5. Publish.
opam install opam-publish
opam publish

# 6. After opam-repository PR is merged.
opam update
opam install ocaml-doctor
ocaml-doctor check
```
