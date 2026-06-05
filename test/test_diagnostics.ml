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

let tool_spec =
  Check.
    {
      command = "tool";
      args = [ "--version" ];
      label = "tool";
      missing_severity = Warn;
      missing_suggestion = "install tool";
      version_parser = String.trim;
    }

let test_command_diagnostic_uses_first_trimmed_stdout_line () =
  let responses =
    [
      ( ("tool", [ "--version" ]),
        (Process.Exited 0, "  1.2.3  \nignored\n", "") );
    ]
  in
  let diagnostic =
    Check.command_diagnostic ~run:(fake_runner responses) tool_spec
  in
  expect_severity "command success" Check.Ok diagnostic.severity;
  expect_string "command title" "tool found: 1.2.3" diagnostic.title

let test_command_diagnostic_uses_stderr_version_when_stdout_is_empty ()
    =
  let responses =
    [
      (("tool", [ "--version" ]), (Process.Exited 0, "", "  9.9.9  \n"));
    ]
  in
  let diagnostic =
    Check.command_diagnostic ~run:(fake_runner responses) tool_spec
  in
  expect_severity "stderr version success" Check.Ok diagnostic.severity;
  expect_string "stderr version title" "tool found: 9.9.9"
    diagnostic.title

let test_command_diagnostic_reports_stderr_only_failure () =
  let responses =
    [
      ( ("tool", [ "--version" ]),
        (Process.Exited 2, "", "tool failed\n") );
    ]
  in
  let diagnostic =
    Check.command_diagnostic ~run:(fake_runner responses) tool_spec
  in
  expect_severity "stderr-only failure" Check.Warn diagnostic.severity;
  expect_string "stderr-only failure title" "tool command failed"
    diagnostic.title;
  expect_detail "stderr-only failure detail"
    "tool --version returned exit 2: tool failed" diagnostic

let test_command_diagnostic_reports_missing_command () =
  let diagnostic =
    Check.command_diagnostic ~run:(fake_runner []) tool_spec
  in
  expect_severity "missing command" Check.Warn diagnostic.severity;
  expect_string "missing command title" "tool not found"
    diagnostic.title;
  expect_detail "missing command detail"
    "The `tool` command is not available on PATH." diagnostic;
  expect_suggestion "missing command suggestion" "install tool"
    diagnostic

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

let test_opam_available_when_version_command_succeeds () =
  let responses =
    [ (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", "")) ]
  in
  if not (Opam.opam_available ~run:(fake_runner responses)) then
    failwith "opam should be available"

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

let test_switch_show_failure_reports_switch_error () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 1, "", "No switch is currently set\n") );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "", "") );
    ]
  in
  let diagnostics =
    Opam.switch_diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let active = find_diagnostic "opam.switch.active" diagnostics in
  expect_severity "switch show failure" Check.Error active.severity;
  expect_detail "switch show failure detail"
    "opam switch show returned exit 1: No switch is currently set"
    active;
  expect_suggestion "switch show failure suggestion"
    "opam switch create 5.2.0" active

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

let test_opam_env_rejects_sibling_path_prefix () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/me/.opam\n", "") );
      (("opam", [ "switch"; "show" ]), (Process.Exited 0, "5.2.0\n", ""));
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "5.2.0\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, "/home/me/.opam/5.2.0/bin\n", "") );
      ( ("sh", [ "-c"; "command -v ocaml" ]),
        (Process.Exited 0, "/home/me/.opam/5.2.0/bin-old/ocaml\n", "")
      );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "ocaml\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "sibling path prefix warning" Check.Warn env.severity;
  expect_contains "sibling path prefix detail"
    "ocaml: /home/me/.opam/5.2.0/bin-old/ocaml"
    (expect_some "sibling path prefix detail" env.detail)

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

let test_windows_where_reports_first_non_switch_match () =
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
        ( Process.Exited 0,
          "C:\\OCaml\\bin\\ocaml.exe\n" ^ switch_bin ^ "\\ocaml.exe\n",
          "" ) );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "ocaml\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Windows
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "windows first match warning" Check.Warn env.severity;
  expect_contains "windows first match detail"
    "ocaml: C:\\OCaml\\bin\\ocaml.exe"
    (expect_some "windows first match detail" env.detail)

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

let test_package_diagnostics_report_installed_and_missing_packages () =
  let diagnostics =
    Opam.package_diagnostics
      (Opam.Installed_packages [ "dune"; "ocamlformat" ])
  in
  let dune = find_diagnostic "opam.package.dune" diagnostics in
  expect_severity "installed package" Check.Ok dune.severity;
  let lsp =
    find_diagnostic "opam.package.ocaml-lsp-server" diagnostics
  in
  expect_severity "missing package" Check.Warn lsp.severity;
  expect_string "missing package title" "ocaml-lsp-server not installed"
    lsp.title;
  expect_suggestion "missing package suggestion"
    "opam install ocaml-lsp-server" lsp

let test_similar_package_name_does_not_count_as_installed () =
  let diagnostics =
    Opam.package_diagnostics
      (Opam.Installed_packages
         [ "dune-configurator"; "ocaml-lsp-server"; "ocamlformat" ])
  in
  let dune = find_diagnostic "opam.package.dune" diagnostics in
  expect_severity "similar package name" Check.Warn dune.severity;
  expect_string "similar package title" "dune not installed" dune.title

let test_package_query_failure_is_reported () =
  let result =
    result ~stderr:"opam failed\n" (Process.Exited 31) "opam"
      [ "list"; "--installed"; "--short" ]
  in
  let diagnostics =
    Opam.package_diagnostics (Opam.Package_query_failed result)
  in
  let diagnostic = find_diagnostic "opam.packages" diagnostics in
  expect_severity "package query failure" Check.Warn diagnostic.severity;
  expect_detail "package query failure detail"
    "opam list --installed --short returned exit 31: opam failed"
    diagnostic

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

let fake_doctor_plugin_runner ?(symlink_state = "enabled") probe_state
    command args =
  match (command, args) with
  | "opam", [ "var"; "root" ] ->
      result ~stdout:"C:\\opam\n" (Process.Exited 0) command args
  | "opam", [ "var"; "bin" ] ->
      result ~stdout:"C:\\opam\\default\\bin\n" (Process.Exited 0)
        command args
  | "powershell", [ _; _; _; _; _; script ]
    when contains_substring script "AppModelUnlock" ->
      result ~stdout:(symlink_state ^ "\n") (Process.Exited 0) command
        args
  | "powershell", _ ->
      result ~stdout:(probe_state ^ "\n") (Process.Exited 0) command
        args
  | _ -> result (Process.Spawn_error "not found") command args

let test_doctor_plugin_probe_script_preserves_powershell_if_chain () =
  let script =
    Opam.doctor_plugin_probe_script ~root:"C:\\opam"
      ~switch_bin:"C:\\opam\\default\\bin"
  in
  expect_contains "plugin probe uses elseif" " } elseif " script;
  if contains_substring script "}; elseif" then
    failwith
      "plugin probe must not separate PowerShell elseif with semicolon"

let test_windows_symlink_probe_reports_enabled () =
  let diagnostics =
    Opam.windows_symlink_diagnostics
      ~run:(fake_doctor_plugin_runner "ok" ~symlink_state:"enabled")
      Platform.Windows
  in
  let symlink = find_diagnostic "opam.windows.symlink" diagnostics in
  expect_severity "windows symlink enabled" Check.Ok symlink.severity;
  expect_string "windows symlink enabled title"
    "Windows user symlink support is enabled" symlink.title

let test_windows_symlink_probe_warns_when_disabled () =
  let diagnostics =
    Opam.windows_symlink_diagnostics
      ~run:(fake_doctor_plugin_runner "ok" ~symlink_state:"disabled")
      Platform.Windows
  in
  let symlink = find_diagnostic "opam.windows.symlink" diagnostics in
  expect_severity "windows symlink disabled" Check.Warn symlink.severity;
  expect_string "windows symlink disabled title"
    "Windows user symlink support may be disabled" symlink.title;
  expect_contains "windows symlink disabled detail"
    "opam plugin entries may be copied instead of linked"
    (expect_some "windows symlink disabled detail" symlink.detail);
  expect_suggestion "windows symlink disabled suggestion"
    "Enable Windows Developer Mode or run `opam reinstall doctor` from \
     an elevated shell."
    symlink

let test_windows_doctor_plugin_entry_is_ok_when_symlink_points_to_switch_bin
    () =
  let diagnostics =
    Opam.doctor_plugin_diagnostics
      ~run:(fake_doctor_plugin_runner "ok")
      Platform.Windows
  in
  let plugin = find_diagnostic "opam.plugin.doctor" diagnostics in
  expect_severity "plugin dispatch ok" Check.Ok plugin.severity;
  expect_string "plugin dispatch ok title"
    "opam doctor plugin dispatch looks usable" plugin.title

let test_windows_doctor_plugin_entry_warns_when_not_symlink () =
  let diagnostics =
    Opam.doctor_plugin_diagnostics
      ~run:(fake_doctor_plugin_runner "not-symlink")
      Platform.Windows
  in
  let plugin = find_diagnostic "opam.plugin.doctor" diagnostics in
  let symlink = find_diagnostic "opam.windows.symlink" diagnostics in
  expect_severity "plugin dispatch copied exe" Check.Warn
    plugin.severity;
  expect_severity "plugin dispatch symlink support" Check.Ok
    symlink.severity;
  expect_string "plugin dispatch copied exe title"
    "opam doctor plugin entry is not a symlink" plugin.title;
  expect_contains "plugin dispatch detail"
    "Plugin entry: C:\\opam\\plugins\\bin\\opam-doctor.exe"
    (expect_some "plugin dispatch detail" plugin.detail);
  expect_contains "plugin dispatch target detail"
    "Switch binary: C:\\opam\\default\\bin\\opam-doctor.exe"
    (expect_some "plugin dispatch detail" plugin.detail);
  expect_suggestion "plugin dispatch copied exe suggestion"
    "Enable Windows Developer Mode or use an elevated shell if symlink \
     creation is unavailable, then run `opam reinstall doctor`."
    plugin

let test_windows_doctor_plugin_entry_warns_when_target_missing () =
  let diagnostics =
    Opam.doctor_plugin_diagnostics
      ~run:(fake_doctor_plugin_runner "target-missing")
      Platform.Windows
  in
  let plugin = find_diagnostic "opam.plugin.doctor" diagnostics in
  expect_severity "plugin dispatch stale target" Check.Warn
    plugin.severity;
  expect_string "plugin dispatch stale target title"
    "opam doctor plugin target missing" plugin.title;
  expect_suggestion "plugin dispatch stale target suggestion"
    "opam reinstall doctor" plugin

let test_doctor_plugin_probe_is_quiet_when_not_installed () =
  let diagnostics =
    Opam.doctor_plugin_diagnostics
      ~run:(fake_doctor_plugin_runner "absent")
      Platform.Windows
  in
  expect_no_diagnostic "opam.plugin.doctor" diagnostics;
  expect_no_diagnostic "opam.windows.symlink" diagnostics

let test_missing_code_command_skips_vscode_extension_check () =
  let diagnostics = Editor.diagnostics ~run:(fake_runner []) in
  let code = find_diagnostic "editor.vscode.command" diagnostics in
  expect_severity "missing code is ok" Check.Ok code.severity

let test_vscode_with_ocaml_platform_extension_is_ok () =
  let responses =
    [
      (("code", [ "--version" ]), (Process.Exited 0, "1.90.0\n", ""));
      ( ("code", [ "--list-extensions" ]),
        (Process.Exited 0, "  ocamllabs.ocaml-platform  \n", "") );
    ]
  in
  let diagnostics = Editor.diagnostics ~run:(fake_runner responses) in
  let extension =
    find_diagnostic "editor.vscode.ocaml-platform" diagnostics
  in
  expect_severity "VS Code extension present" Check.Ok
    extension.severity;
  expect_string "VS Code extension present title"
    "VS Code OCaml Platform extension detected" extension.title

let test_similar_vscode_extension_name_does_not_match () =
  let responses =
    [
      (("code", [ "--version" ]), (Process.Exited 0, "1.90.0\n", ""));
      ( ("code", [ "--list-extensions" ]),
        (Process.Exited 0, "ocamllabs.ocaml-platform-insiders\n", "") );
    ]
  in
  let diagnostics = Editor.diagnostics ~run:(fake_runner responses) in
  let extension =
    find_diagnostic "editor.vscode.ocaml-platform" diagnostics
  in
  expect_severity "similar VS Code extension name" Check.Warn
    extension.severity;
  expect_string "similar VS Code extension title"
    "VS Code OCaml Platform extension not detected" extension.title

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

let test_vscode_extension_query_failure_warns () =
  let responses =
    [
      (("code", [ "--version" ]), (Process.Exited 0, "1.90.0\n", ""));
      ( ("code", [ "--list-extensions" ]),
        (Process.Exited 1, "", "extensions unavailable\n") );
    ]
  in
  let diagnostics = Editor.diagnostics ~run:(fake_runner responses) in
  let extensions =
    find_diagnostic "editor.vscode.extensions" diagnostics
  in
  expect_severity "VS Code extension query failure" Check.Warn
    extensions.severity;
  expect_detail "VS Code extension query failure detail"
    "code --list-extensions returned exit 1: extensions unavailable"
    extensions

let () =
  List.iter
    (fun test -> test ())
    [
      test_command_diagnostic_uses_first_trimmed_stdout_line;
      test_command_diagnostic_uses_stderr_version_when_stdout_is_empty;
      test_command_diagnostic_reports_stderr_only_failure;
      test_command_diagnostic_reports_missing_command;
      test_command_checks_use_ocamllsp_fallback;
      test_missing_ocamlformat_is_a_warning;
      test_missing_development_tools_are_warnings;
      test_failed_opam_version_check_is_an_error;
      test_missing_opam_skips_opam_checks_as_error;
      test_opam_available_when_version_command_succeeds;
      test_opam_not_initialized_reports_warning;
      test_switch_show_failure_reports_switch_error;
      test_opam_without_selected_switch_reports_switch_error;
      test_error_like_switch_show_output_reports_no_active_switch;
      test_opam_env_warns_when_ocaml_resolves_outside_active_switch;
      test_opam_env_rejects_sibling_path_prefix;
      test_opam_env_warns_when_installed_switch_tools_are_missing_from_path;
      test_opam_env_is_ok_when_installed_switch_tools_are_visible;
      test_windows_where_reports_first_non_switch_match;
      test_empty_opam_bin_output_is_reported;
      test_package_diagnostics_report_installed_and_missing_packages;
      test_similar_package_name_does_not_count_as_installed;
      test_package_query_failure_is_reported;
      test_windows_opam_env_suggestion_matches_shell_wording;
      test_doctor_plugin_probe_script_preserves_powershell_if_chain;
      test_windows_symlink_probe_reports_enabled;
      test_windows_symlink_probe_warns_when_disabled;
      test_windows_doctor_plugin_entry_is_ok_when_symlink_points_to_switch_bin;
      test_windows_doctor_plugin_entry_warns_when_not_symlink;
      test_windows_doctor_plugin_entry_warns_when_target_missing;
      test_doctor_plugin_probe_is_quiet_when_not_installed;
      test_missing_code_command_skips_vscode_extension_check;
      test_vscode_with_ocaml_platform_extension_is_ok;
      test_similar_vscode_extension_name_does_not_match;
      test_vscode_without_ocaml_platform_extension_warns;
      test_vscode_extension_query_failure_warns;
    ]
