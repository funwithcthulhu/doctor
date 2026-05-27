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

let contains_substring haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop index =
    needle_length = 0
    || index + needle_length <= haystack_length
       && (String.sub haystack index needle_length = needle
          || loop (index + 1))
  in
  loop 0

let expect_contains label needle haystack =
  if not (contains_substring haystack needle) then
    failwith (Printf.sprintf "%s: missing substring %S" label needle)

let find_diagnostic id diagnostics =
  diagnostics
  |> List.find_opt (fun diagnostic ->
      String.equal diagnostic.Check.id id)
  |> expect_some ("diagnostic " ^ id)

let expect_no_diagnostic id diagnostics =
  if
    List.exists
      (fun diagnostic -> String.equal diagnostic.Check.id id)
      diagnostics
  then failwith (Printf.sprintf "unexpected diagnostic %s" id)

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

let test_opam_not_initialized_reports_warning () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 1, "", "opam has not been initialized\n") );
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 1, "", "No switch is currently set\n") );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 1, "", "opam has not been initialized\n") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 1, "", "opam has not been initialized\n") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let initialized = find_diagnostic "opam.initialized" diagnostics in
  expect_severity "uninitialized opam is warning" Check.Warn
    initialized.severity;
  expect_string "uninitialized opam title"
    "opam does not appear initialized" initialized.title;
  expect_detail "uninitialized opam detail"
    "opam var root returned exit 1: opam has not been initialized"
    initialized;
  expect_suggestion "uninitialized opam suggestion" "opam init"
    initialized

let test_opam_without_selected_switch_reports_switch_error () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/me/.opam\n", "") );
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 1, "", "No switch is currently set\n") );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n5.2.0\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let active = find_diagnostic "opam.switch.active" diagnostics in
  expect_severity "inactive switch is error" Check.Error active.severity;
  expect_string "inactive switch title" "opam switch not active"
    active.title;
  expect_suggestion "inactive switch suggestion" "eval $(opam env)"
    active;
  expect_no_diagnostic "opam.env.sync" diagnostics;
  expect_int "inactive switch exit code" 2 (Check.exit_code diagnostics)

let test_error_like_switch_show_output_reports_no_active_switch () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/me/.opam\n", "") );
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 0, "[ERROR] No switch is currently set\n", "")
      );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let active = find_diagnostic "opam.switch.active" diagnostics in
  expect_severity "error-like switch show output" Check.Error
    active.severity;
  expect_string "error-like switch show title" "opam switch not active"
    active.title;
  expect_detail "error-like switch show detail"
    "opam did not report an active switch." active;
  expect_no_diagnostic "opam.env.sync" diagnostics

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
        (Process.Exited 0, "ocaml\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "env sync warning" Check.Warn env.severity;
  expect_string "env sync title"
    "shell environment may not include the active opam switch" env.title;
  expect_detail "env sync detail"
    "Active switch bin: /home/me/.opam/5.2.0/bin\n\
     Commands resolving outside the active switch:\n\
     ocaml: /usr/bin/ocaml"
    env;
  expect_suggestion "env sync suggestion" "eval $(opam env)" env;
  let ocamlformat =
    find_diagnostic "opam.package.ocamlformat" diagnostics
  in
  expect_severity "missing ocamlformat package" Check.Warn
    ocamlformat.severity

let test_opam_env_warns_when_installed_switch_tools_are_missing_from_path
    () =
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
  expect_severity "missing switch tools warning" Check.Warn env.severity;
  expect_string "missing switch tools title"
    "shell environment may not include the active opam switch" env.title;
  expect_contains "missing switch tools detail"
    "Active switch bin: C:\\opam\\default\\bin"
    (expect_some "missing switch tools detail" env.detail);
  expect_contains "missing commands detail"
    "Commands missing from PATH: ocaml, dune, OCaml LSP, ocamlformat."
    (expect_some "missing switch tools detail" env.detail);
  expect_contains "powershell env suggestion"
    "PowerShell: (& opam env) -split"
    (expect_some "missing switch tools suggestion" env.suggestion);
  let json = Doctor.Report.render_json diagnostics in
  expect_contains "env json name" "\"name\": \"opam.env.sync\"" json;
  expect_contains "env json status" "\"status\": \"warn\"" json;
  expect_contains "env json message"
    "\"message\": \"shell environment may not include the active opam \
     switch\""
    json;
  expect_contains "env json detail"
    "\"Active switch bin: C:\\\\opam\\\\default\\\\bin\"" json;
  expect_contains "env json suggestion"
    "\"Suggested fix: PowerShell: (& opam env) -split" json;
  let dune = find_diagnostic "opam.package.dune" diagnostics in
  expect_severity "dune package installed" Check.Ok dune.severity

let test_opam_env_is_ok_when_installed_switch_tools_are_visible () =
  let switch_bin = "C:\\opam\\default\\bin" in
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      (("opam", [ "var"; "root" ]), (Process.Exited 0, "C:\\opam\n", ""));
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, switch_bin ^ "\n", "") );
      ( ("where", [ "ocaml" ]),
        (Process.Exited 0, switch_bin ^ "\\ocaml.exe\n", "") );
      ( ("where", [ "dune" ]),
        (Process.Exited 0, switch_bin ^ "\\dune.exe\n", "") );
      ( ("where", [ "ocaml-lsp-server" ]),
        (Process.Exited 0, switch_bin ^ "\\ocaml-lsp-server.exe\n", "")
      );
      ( ("where", [ "ocamlformat" ]),
        (Process.Exited 0, switch_bin ^ "\\ocamlformat.exe\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        ( Process.Exited 0,
          "ocaml\ndune\nocaml-lsp-server\nocamlformat\nutop\n",
          "" ) );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Windows
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "visible switch tools ok" Check.Ok env.severity;
  expect_string "visible switch tools title"
    "shell environment appears synced with opam" env.title;
  expect_detail "visible switch tools detail"
    "ocaml resolves to C:\\opam\\default\\bin\\ocaml.exe" env;
  let ocamlformat =
    find_diagnostic "opam.package.ocamlformat" diagnostics
  in
  expect_severity "ocamlformat package installed" Check.Ok
    ocamlformat.severity

let test_empty_opam_bin_output_is_reported () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/me/.opam\n", "") );
      (("opam", [ "switch"; "show" ]), (Process.Exited 0, "5.2.0\n", ""));
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "5.2.0\n", "") );
      (("opam", [ "var"; "bin" ]), (Process.Exited 0, "\n", ""));
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "empty opam bin warning" Check.Warn env.severity;
  expect_string "empty opam bin title"
    "active switch bin could not be read" env.title;
  expect_detail "empty opam bin detail" "opam var bin returned exit 0"
    env;
  expect_suggestion "empty opam bin suggestion"
    "Run `opam var bin` to inspect the active switch." env

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
    "PowerShell: (& opam env) -split '\\r?\\n' | ForEach-Object { \
     Invoke-Expression $_ }\n\
     cmd.exe: for /f \"tokens=*\" %i in ('opam env') do @%i"
    env

let test_missing_code_command_skips_vscode_extension_check () =
  let diagnostics = Editor.diagnostics ~run:(fake_runner []) in
  let code = find_diagnostic "editor.vscode.command" diagnostics in
  expect_severity "missing code is ok" Check.Ok code.severity

let test_vscode_without_ocaml_platform_extension_warns () =
  let responses =
    [
      (("code", [ "--version" ]), (Process.Exited 0, "1.90.0\n", ""));
      ( ("code", [ "--list-extensions" ]),
        (Process.Exited 0, "some.other-extension\n", "") );
    ]
  in
  let diagnostics = Editor.diagnostics ~run:(fake_runner responses) in
  let extension =
    find_diagnostic "editor.vscode.ocaml-platform" diagnostics
  in
  expect_severity "missing VS Code extension is warning" Check.Warn
    extension.severity;
  expect_string "missing VS Code extension title"
    "VS Code OCaml Platform extension not detected" extension.title;
  expect_suggestion "missing VS Code extension suggestion"
    "Install extension ocamllabs.ocaml-platform in VS Code." extension

let () =
  List.iter
    (fun test -> test ())
    [
      test_command_checks_use_ocamllsp_fallback;
      test_missing_ocamlformat_is_a_warning;
      test_missing_development_tools_are_warnings;
      test_failed_opam_version_check_is_an_error;
      test_missing_opam_skips_opam_checks_as_error;
      test_opam_not_initialized_reports_warning;
      test_opam_without_selected_switch_reports_switch_error;
      test_error_like_switch_show_output_reports_no_active_switch;
      test_opam_env_warns_when_ocaml_resolves_outside_active_switch;
      test_opam_env_warns_when_installed_switch_tools_are_missing_from_path;
      test_opam_env_is_ok_when_installed_switch_tools_are_visible;
      test_empty_opam_bin_output_is_reported;
      test_windows_opam_env_suggestion_matches_shell_wording;
      test_missing_code_command_skips_vscode_extension_check;
      test_vscode_without_ocaml_platform_extension_warns;
    ]
