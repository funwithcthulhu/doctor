# Diagnostic contract

`doctor check` is a read-only diagnostic command for local OCaml development
environments. It reports what it can observe from the current process
environment and the commands available on `PATH`.

## Checks

The current checks cover:

- detected platform;
- core commands: `opam`, `ocaml`, `dune`, `ocaml-lsp-server` or `ocamllsp`,
  and `ocamlformat`;
- whether opam appears initialized;
- active and available opam switches;
- whether the resolved `ocaml` and installed switch tools appear to come from
  the active switch;
- selected opam packages used by ordinary OCaml development;
- the VS Code OCaml Platform extension, when the `code` command is available.

The command may skip checks when a prerequisite command is missing. For
example, opam-specific checks are skipped if `opam` cannot be run.

## Non-mutating behavior

`doctor check` does not modify the machine it inspects. In particular, it does
not:

- run `opam init`;
- create, remove, or select opam switches;
- install opam packages;
- edit shell startup files;
- edit VS Code settings or install editor extensions;
- write project files.

Suggested fixes are printed as text for the user to run deliberately.

## Exit codes

- `0`: no warnings or errors were reported.
- `1`: one or more warnings were reported, but no errors.
- `2`: one or more errors were reported.
- `3`: `doctor` hit an unexpected internal failure.

Warnings usually mean the environment is usable but incomplete or suspicious.
Errors mean a required command or environment state is missing.

## JSON output

`doctor check --json` prints the same diagnostics as the text report using a
stable shape:

```json
{
  "summary": {
    "status": "ok",
    "exit_code": 0
  },
  "diagnostics": []
}
```

The `summary.exit_code` value matches the process exit code that would be used
for the same diagnostics. The `diagnostics[].name` values are intended to be
stable enough for scripts. Human-readable messages and details should still be
treated as display text; tools should prefer `name`, `status`, and
`summary.exit_code` for branching.

## Diagnostic names

| Name | Meaning |
| --- | --- |
| `platform.os` | Detected platform. |
| `command.opam` | `opam` command availability and version. |
| `command.ocaml` | `ocaml` command availability and version. |
| `command.dune` | `dune` command availability and version. |
| `command.ocaml-lsp-server` | OCaml LSP command availability and version. |
| `command.ocamlformat` | `ocamlformat` command availability and version. |
| `opam.initialized` | Whether opam appears initialized. |
| `opam.switch.active` | Active opam switch state. |
| `opam.switch.list` | Available opam switches. |
| `opam.env.sync` | Whether visible switch tools match the active switch. |
| `opam.plugin.doctor` | Windows opam plugin dispatch state for `opam doctor`. |
| `opam.windows.symlink` | Windows user symlink support for opam plugin entries. |
| `opam.package.dune` | Installed `dune` package state. |
| `opam.package.ocaml-lsp-server` | Installed `ocaml-lsp-server` package state. |
| `opam.package.ocamlformat` | Installed `ocamlformat` package state. |
| `opam.package.utop` | Installed optional `utop` package state. |
| `opam.packages` | Failure to read installed opam packages. |
| `editor.vscode.command` | VS Code command availability. |
| `editor.vscode.ocaml-platform` | VS Code OCaml Platform extension state. |
| `editor.vscode.extensions` | Failure to read VS Code extensions. |
