module Check = Doctor.Check
module Editor = Doctor.Editor
module Opam = Doctor.Opam
module Platform = Doctor.Platform
module Process = Doctor.Process

let result ?(stdout = "") ?(stderr = "") status command args =
  { Process.command; args; status; stdout; stderr }

let fake_runner responses command args =
  match List.assoc_opt (command, args) responses with
  | Some (status, stdout, stderr) ->
      result ~stdout ~stderr status command args
  | None -> result (Process.Spawn_error "not found") command args

let expect_string label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_int label expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let expect_severity label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: wrong severity" label)

let expect_some label = function
  | Some value -> value
  | None -> failwith (Printf.sprintf "%s: expected Some _" label)

let expect_suggestion label expected diagnostic =
  expect_string label expected
    (expect_some (label ^ " suggestion") diagnostic.Check.suggestion)

let expect_detail label expected diagnostic =
  expect_string label expected
    (expect_some (label ^ " detail") diagnostic.Check.detail)

let find_diagnostic id diagnostics =
  diagnostics
  |> List.find_opt (fun diagnostic ->
      String.equal diagnostic.Check.id id)
  |> expect_some ("diagnostic " ^ id)

let test_command_checks_use_ocamllsp_fallback () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("ocaml", [ "-version" ]),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      ( ("ocaml-lsp-server", [ "--version" ]),
        (Process.Spawn_error "not found", "", "") );
      (("ocamllsp", [ "--version" ]), (Process.Exited 0, "1.26.0\n", ""));
      ( ("ocamlformat", [ "--version" ]),
        (Process.Exited 0, "0.27.0\n", "") );
    ]
  in
  let diagnostics =
    Check.command_diagnostics ~run:(fake_runner responses)
  in
  let lsp = find_diagnostic "command.ocaml-lsp-server" diagnostics in
  expect_severity "lsp fallback" Check.Ok lsp.severity;
  expect_string "lsp fallback title"
    "OCaml LSP found: 1.26.0 (ocamllsp)" lsp.title

let test_missing_ocamlformat_is_a_warning () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("ocaml", [ "-version" ]),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      ( ("ocaml-lsp-server", [ "--version" ]),
        (Process.Exited 0, "1.26.0\n", "") );
    ]
  in
  let diagnostics =
    Check.command_diagnostics ~run:(fake_runner responses)
  in
  let diagnostic = find_diagnostic "command.ocamlformat" diagnostics in
  expect_severity "missing command is warning" Check.Warn
    diagnostic.severity;
  expect_string "missing command suggestion" "opam install ocamlformat"
    (expect_some "missing command suggestion" diagnostic.suggestion)

let test_missing_development_tools_are_warnings () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("ocaml", [ "-version" ]),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      ( ("dune", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
      ( ("ocaml-lsp-server", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
      ( ("ocamllsp", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
      ( ("ocamlformat", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
    ]
  in
  let diagnostics =
    Check.command_diagnostics ~run:(fake_runner responses)
  in
  let dune = find_diagnostic "command.dune" diagnostics in
  expect_severity "missing dune is warning" Check.Warn dune.severity;
  expect_string "missing dune title" "dune not found" dune.title;
  expect_suggestion "missing dune suggestion" "opam install dune" dune;
  let lsp = find_diagnostic "command.ocaml-lsp-server" diagnostics in
  expect_severity "missing lsp is warning" Check.Warn lsp.severity;
  expect_string "missing lsp title" "OCaml LSP command not found"
    lsp.title;
  expect_detail "missing lsp detail"
    "Checked `ocaml-lsp-server` and `ocamllsp`; neither command is \
     available on PATH."
    lsp;
  expect_suggestion "missing lsp suggestion"
    "opam install ocaml-lsp-server" lsp;
  let ocamlformat = find_diagnostic "command.ocamlformat" diagnostics in
  expect_severity "missing ocamlformat is warning" Check.Warn
    ocamlformat.severity;
  expect_string "missing ocamlformat title" "ocamlformat not found"
    ocamlformat.title;
  expect_suggestion "missing ocamlformat suggestion"
    "opam install ocamlformat" ocamlformat;
  expect_int "missing tools exit code" 1 (Check.exit_code diagnostics)

let test_failed_opam_version_check_is_an_error () =
  let responses =
    [
      ( ("opam", [ "--version" ]),
        (Process.Exited 2, "", "opam failed\n") );
      ( ("ocaml", [ "-version" ]),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      ( ("ocaml-lsp-server", [ "--version" ]),
        (Process.Exited 0, "1.26.0\n", "") );
      ( ("ocamlformat", [ "--version" ]),
        (Process.Exited 0, "0.27.0\n", "") );
    ]
  in
  let diagnostics =
    Check.command_diagnostics ~run:(fake_runner responses)
  in
  let diagnostic = find_diagnostic "command.opam" diagnostics in
  expect_severity "nonzero command is diagnostic" Check.Error
    diagnostic.severity

let test_missing_opam_skips_opam_checks_as_error () =
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner []) Platform.Linux
  in
  let diagnostic = find_diagnostic "opam.initialized" diagnostics in
  expect_severity "missing opam is error" Check.Error
    diagnostic.severity;
  expect_string "missing opam title"
    "opam checks skipped because opam is missing" diagnostic.title;
  expect_suggestion "missing opam suggestion"
    "Install opam from https://opam.ocaml.org/doc/Install.html"
    diagnostic;
  expect_int "missing opam exit code" 2 (Check.exit_code diagnostics)

let test_opam_env_warns_when_ocaml_resolves_outside_active_switch () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/me/.opam\n", "") );
      (("opam", [ "switch"; "show" ]), (Process.Exited 0, "5.2.0\n", ""));
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n5.2.0\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, "/home/me/.opam/5.2.0/bin\n", "") );
      ( ("sh", [ "-c"; "command -v ocaml" ]),
        (Process.Exited 0, "/usr/bin/ocaml\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "ocaml\ndune\nocaml-lsp-server\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "env sync warning" Check.Warn env.severity;
  expect_string "env sync title"
    "shell environment may be out of sync with opam" env.title;
  expect_detail "env sync detail"
    "ocaml resolves to /usr/bin/ocaml, but the active switch bin is \
     /home/me/.opam/5.2.0/bin."
    env;
  expect_suggestion "env sync suggestion" "eval $(opam env)" env;
  let ocamlformat =
    find_diagnostic "opam.package.ocamlformat" diagnostics
  in
  expect_severity "missing ocamlformat package" Check.Warn
    ocamlformat.severity

let test_windows_opam_env_suggestion_matches_shell_wording () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      (("opam", [ "var"; "root" ]), (Process.Exited 0, "C:\\opam\n", ""));
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, "C:\\opam\\default\\bin\n", "") );
      ( ("where", [ "ocaml" ]),
        (Process.Exited 0, "C:\\OCaml\\bin\\ocaml.exe\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        ( Process.Exited 0,
          "ocaml\ndune\nocaml-lsp-server\nocamlformat\n",
          "" ) );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Windows
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "windows env sync warning" Check.Warn env.severity;
  expect_suggestion "windows env sync suggestion"
    "Run `opam env` and apply the environment changes in your current \
     shell, then restart the terminal if needed."
    env

let test_missing_code_command_skips_vscode_extension_check () =
  let diagnostics = Editor.diagnostics ~run:(fake_runner []) in
  let code = find_diagnostic "editor.vscode.command" diagnostics in
  expect_severity "missing code is ok" Check.Ok code.severity

let () =
  List.iter
    (fun test -> test ())
    [
      test_command_checks_use_ocamllsp_fallback;
      test_missing_ocamlformat_is_a_warning;
      test_missing_development_tools_are_warnings;
      test_failed_opam_version_check_is_an_error;
      test_missing_opam_skips_opam_checks_as_error;
      test_opam_env_warns_when_ocaml_resolves_outside_active_switch;
      test_windows_opam_env_suggestion_matches_shell_wording;
      test_missing_code_command_skips_vscode_extension_check;
    ]
