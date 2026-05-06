module Check = Doctor.Check
module Editor = Doctor.Editor
module Opam = Doctor.Opam
module Platform = Doctor.Platform
module Process = Doctor.Process

let result ?(stdout = "") ?(stderr = "") status command args =
  { Process.command; args; status; stdout; stderr }

let fake_runner responses command args =
  match List.assoc_opt (command, args) responses with
  | Some (status, stdout, stderr) -> result ~stdout ~stderr status command args
  | None -> result (Process.Spawn_error "not found") command args

let expect_string label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_severity label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: wrong severity" label)

let expect_some label = function
  | Some value -> value
  | None -> failwith (Printf.sprintf "%s: expected Some _" label)

let find_diagnostic id diagnostics =
  diagnostics
  |> List.find_opt (fun diagnostic -> String.equal diagnostic.Check.id id)
  |> expect_some ("diagnostic " ^ id)

let test_command_checks_use_ocamllsp_fallback () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ( "ocaml",
          [ "-version" ] ),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      ( ( "ocaml-lsp-server",
          [ "--version" ] ),
        (Process.Spawn_error "not found", "", "") );
      (("ocamllsp", [ "--version" ]), (Process.Exited 0, "1.26.0\n", ""));
      (("ocamlformat", [ "--version" ]), (Process.Exited 0, "0.27.0\n", ""));
    ]
  in
  let diagnostics = Check.command_diagnostics ~run:(fake_runner responses) in
  let lsp = find_diagnostic "command.ocaml-lsp-server" diagnostics in
  expect_severity "lsp fallback" Check.Ok lsp.severity;
  expect_string "lsp fallback title" "OCaml LSP found: 1.26.0 (ocamllsp)"
    lsp.title

let test_missing_ocamlformat_is_a_warning () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ( "ocaml",
          [ "-version" ] ),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      (("ocaml-lsp-server", [ "--version" ]), (Process.Exited 0, "1.26.0\n", ""));
    ]
  in
  let diagnostics = Check.command_diagnostics ~run:(fake_runner responses) in
  let diagnostic = find_diagnostic "command.ocamlformat" diagnostics in
  expect_severity "missing command is warning" Check.Warn diagnostic.severity;
  expect_string "missing command suggestion" "opam install ocamlformat"
    (expect_some "missing command suggestion" diagnostic.suggestion)

let test_failed_opam_version_check_is_an_error () =
  let responses =
    [
      ( ( "opam",
          [ "--version" ] ),
        (Process.Exited 2, "", "opam failed\n") );
      ( ( "ocaml",
          [ "-version" ] ),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      (("ocaml-lsp-server", [ "--version" ]), (Process.Exited 0, "1.26.0\n", ""));
      (("ocamlformat", [ "--version" ]), (Process.Exited 0, "0.27.0\n", ""));
    ]
  in
  let diagnostics = Check.command_diagnostics ~run:(fake_runner responses) in
  let diagnostic = find_diagnostic "command.opam" diagnostics in
  expect_severity "nonzero command is diagnostic" Check.Error
    diagnostic.severity

let test_opam_env_warns_when_ocaml_resolves_outside_active_switch () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      (("opam", [ "var"; "root" ]), (Process.Exited 0, "/home/me/.opam\n", ""));
      (("opam", [ "switch"; "show" ]), (Process.Exited 0, "5.2.0\n", ""));
      ( ( "opam",
          [ "switch"; "list"; "--short" ] ),
        (Process.Exited 0, "default\n5.2.0\n", "") );
      ( ( "opam",
          [ "var"; "bin" ] ),
        (Process.Exited 0, "/home/me/.opam/5.2.0/bin\n", "") );
      ( ( "sh",
          [ "-c"; "command -v ocaml" ] ),
        (Process.Exited 0, "/usr/bin/ocaml\n", "") );
      ( ( "opam",
          [ "list"; "--installed"; "--short" ] ),
        ( Process.Exited 0,
          "ocaml\ndune\nocaml-lsp-server\n",
          "" ) );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "env sync warning" Check.Warn env.severity;
  expect_string "env sync suggestion" "eval $(opam env)"
    (expect_some "env sync suggestion" env.suggestion);
  let ocamlformat = find_diagnostic "opam.package.ocamlformat" diagnostics in
  expect_severity "missing ocamlformat package" Check.Warn ocamlformat.severity

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
      test_failed_opam_version_check_is_an_error;
      test_opam_env_warns_when_ocaml_resolves_outside_active_switch;
      test_missing_code_command_skips_vscode_extension_check;
    ]
