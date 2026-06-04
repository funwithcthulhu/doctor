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

let expect_int label expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let expect_string label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

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

let expect_not_contains label needle haystack =
  if contains_substring haystack needle then
    failwith (Printf.sprintf "%s: unexpected substring %S" label needle)

let expect_some label = function
  | Some value -> value
  | None -> failwith (Printf.sprintf "%s: expected Some _" label)

let expect_severity label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: wrong severity" label)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let normalize_newlines text =
  text |> String.split_on_char '\r' |> String.concat ""

let find_project_file relative_path =
  let candidates =
    [
      relative_path;
      Filename.concat ".." relative_path;
      Filename.concat (Filename.concat ".." "..") relative_path;
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None ->
      failwith
        (Printf.sprintf "project file %s not found" relative_path)

let read_project_file relative_path =
  read_file (find_project_file relative_path) |> normalize_newlines

let remove_final_newline text =
  if String.ends_with ~suffix:"\n" text then
    String.sub text 0 (String.length text - 1)
  else text

let read_project_text_fixture relative_path =
  read_project_file relative_path |> remove_final_newline

let single_warning_diagnostic =
  Check.make ~id:"command.ocamlformat"
    ~title:"ocamlformat not installed"
    ~detail:"The `ocamlformat` command is not available on PATH."
    ~suggestion:"opam install ocamlformat" Check.Warn

let single_error_diagnostic =
  Check.make ~id:"opam.switch.active" ~title:"opam switch not active"
    ~detail:"opam did not report an active switch."
    ~suggestion:"eval $(opam env)" Check.Error

let test_golden_json_for_one_warning () =
  expect_int "one-warning exit code" 1
    (Doctor.Report.exit_code [ single_warning_diagnostic ]);
  expect_string "one-warning json"
    (read_project_file "test/fixtures/report_one_warning.json")
    (Doctor.Report.render_json [ single_warning_diagnostic ])

let test_golden_json_for_error_run () =
  expect_int "error exit code" 2
    (Doctor.Report.exit_code [ single_error_diagnostic ]);
  let json = Doctor.Report.render_json [ single_error_diagnostic ] in
  expect_string "error json"
    (read_project_file "test/fixtures/report_error_run.json")
    json;
  expect_contains "error json name key" "\"name\"" json;
  expect_contains "error json status key" "\"status\"" json;
  expect_contains "error json message key" "\"message\"" json;
  expect_contains "error json details key" "\"details\"" json

let find_diagnostic id diagnostics =
  diagnostics
  |> List.find_opt (fun diagnostic ->
      String.equal diagnostic.Check.id id)
  |> expect_some ("diagnostic " ^ id)

let test_opam_env_mismatch_reports_active_switch_bin () =
  let responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/dev/.opam\n", "") );
      (("opam", [ "switch"; "show" ]), (Process.Exited 0, "5.2.0\n", ""));
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "5.2.0\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, "/home/dev/.opam/5.2.0/bin\n", "") );
      ( ("sh", [ "-c"; "command -v ocaml" ]),
        (Process.Exited 0, "/usr/local/bin/ocaml\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "ocaml\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(fake_runner responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "opam env mismatch" Check.Warn env.severity;
  expect_string "opam env mismatch title"
    "shell environment may not include the active opam switch" env.title;
  let detail = expect_some "opam env mismatch detail" env.detail in
  expect_contains "active switch bin detail"
    "Active switch bin: /home/dev/.opam/5.2.0/bin" detail;
  expect_contains "wrong ocaml detail" "ocaml: /usr/local/bin/ocaml"
    detail

let documented_diagnostic_names () =
  read_project_file "docs/diagnostic-contract.md"
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.split_on_char '`' line with
      | _prefix :: name :: _ when String.starts_with ~prefix:"| " line
        ->
          Some name
      | _ -> None)

let add_unique values value =
  if List.exists (String.equal value) values then values
  else value :: values

let sort_unique values =
  values |> List.fold_left add_unique [] |> List.sort String.compare

let diagnostic_names diagnostics =
  diagnostics
  |> List.fold_left
       (fun names diagnostic -> add_unique names diagnostic.Check.id)
       []

let complete_opam_responses =
  [
    (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
    (("opam", [ "var"; "root" ]), (Process.Exited 0, "/tmp/opam\n", ""));
    (("opam", [ "switch"; "show" ]), (Process.Exited 0, "default\n", ""));
    ( ("opam", [ "switch"; "list"; "--short" ]),
      (Process.Exited 0, "default\n", "") );
    ( ("opam", [ "var"; "bin" ]),
      (Process.Exited 0, "/tmp/opam/default/bin\n", "") );
    ( ("sh", [ "-c"; "command -v ocaml" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/ocaml\n", "") );
    ( ("sh", [ "-c"; "command -v dune" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/dune\n", "") );
    ( ("sh", [ "-c"; "command -v ocaml-lsp-server" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/ocaml-lsp-server\n", "")
    );
    ( ("sh", [ "-c"; "command -v ocamlformat" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/ocamlformat\n", "") );
    ( ("opam", [ "list"; "--installed"; "--short" ]),
      ( Process.Exited 0,
        "dune\nocaml-lsp-server\nocamlformat\nutop\n",
        "" ) );
  ]

let package_query_failure_responses =
  [
    (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
    (("opam", [ "var"; "root" ]), (Process.Exited 0, "/tmp/opam\n", ""));
    (("opam", [ "switch"; "show" ]), (Process.Exited 0, "default\n", ""));
    ( ("opam", [ "switch"; "list"; "--short" ]),
      (Process.Exited 0, "default\n", "") );
    ( ("opam", [ "list"; "--installed"; "--short" ]),
      (Process.Exited 12, "", "opam list failed\n") );
  ]

let vscode_extension_failure_responses =
  [
    (("code", [ "--version" ]), (Process.Exited 0, "1.90.0\n", ""));
    ( ("code", [ "--list-extensions" ]),
      (Process.Exited 1, "", "extension query failed\n") );
  ]

let vscode_extension_missing_responses =
  [
    (("code", [ "--version" ]), (Process.Exited 0, "1.90.0\n", ""));
    (("code", [ "--list-extensions" ]), (Process.Exited 0, "\n", ""));
  ]

let emitted_diagnostic_names () =
  [
    [ Platform.diagnostic Platform.Linux ];
    Check.command_diagnostics ~run:(fake_runner []);
    Opam.diagnostics
      ~run:(fake_runner complete_opam_responses)
      Platform.Linux;
    Opam.diagnostics
      ~run:(fake_runner package_query_failure_responses)
      Platform.Linux;
    Editor.diagnostics ~run:(fake_runner []);
    Editor.diagnostics
      ~run:(fake_runner vscode_extension_failure_responses);
    Editor.diagnostics
      ~run:(fake_runner vscode_extension_missing_responses);
  ]
  |> List.concat |> diagnostic_names |> sort_unique

let legacy_diagnostic_names = []

let missing_from left right =
  left
  |> List.filter (fun name ->
      not (List.exists (String.equal name) right))

let test_emitted_diagnostic_names_are_documented () =
  let documented = documented_diagnostic_names () |> sort_unique in
  let emitted = emitted_diagnostic_names () in
  let undocumented = missing_from emitted documented in
  let unused =
    missing_from documented (emitted @ legacy_diagnostic_names)
  in
  match (undocumented, unused) with
  | [], [] -> ()
  | _ :: _, _ ->
      failwith
        (Printf.sprintf
           "emitted diagnostics missing from \
            docs/diagnostic-contract.md: %s"
           (String.concat ", " undocumented))
  | [], _ :: _ ->
      failwith
        (Printf.sprintf
           "documented diagnostics not emitted or marked legacy: %s"
           (String.concat ", " unused))

let capture_output f =
  let stdout_path = Filename.temp_file "doctor-stdout" ".txt" in
  let stderr_path = Filename.temp_file "doctor-stderr" ".txt" in
  let stdout_fd =
    Unix.openfile stdout_path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o600
  in
  let saved_stdout = Unix.dup Unix.stdout in
  let saved_stderr = Unix.dup Unix.stderr in
  let restore () =
    flush stdout;
    flush stderr;
    Unix.dup2 saved_stdout Unix.stdout;
    Unix.dup2 saved_stderr Unix.stderr;
    Unix.close saved_stdout;
    Unix.close saved_stderr;
    Unix.close stdout_fd;
    Unix.close stderr_fd
  in
  flush stdout;
  flush stderr;
  Unix.dup2 stdout_fd Unix.stdout;
  Unix.dup2 stderr_fd Unix.stderr;
  let result =
    match f () with value -> Ok value | exception exn -> Error exn
  in
  restore ();
  let captured_stdout = read_file stdout_path |> normalize_newlines in
  let captured_stderr = read_file stderr_path |> normalize_newlines in
  Sys.remove stdout_path;
  Sys.remove stderr_path;
  match result with
  | Ok value -> (value, captured_stdout, captured_stderr)
  | Error exn -> raise exn

let json_cli_responses =
  [
    (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
    ( ("ocaml", [ "-version" ]),
      (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
    (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
    ( ("ocaml-lsp-server", [ "--version" ]),
      (Process.Exited 0, "1.26.0\n", "") );
    ( ("ocamlformat", [ "--version" ]),
      (Process.Exited 0, "0.29.0\n", "") );
    (("opam", [ "var"; "root" ]), (Process.Exited 0, "/tmp/opam\n", ""));
    (("opam", [ "switch"; "show" ]), (Process.Exited 0, "default\n", ""));
    ( ("opam", [ "switch"; "list"; "--short" ]),
      (Process.Exited 0, "default\n", "") );
    ( ("opam", [ "var"; "bin" ]),
      (Process.Exited 0, "/tmp/opam/default/bin\n", "") );
    ( ("sh", [ "-c"; "command -v ocaml" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/ocaml\n", "") );
    ( ("sh", [ "-c"; "command -v dune" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/dune\n", "") );
    ( ("sh", [ "-c"; "command -v ocaml-lsp-server" ]),
      (Process.Exited 0, "/tmp/opam/default/bin/ocaml-lsp-server\n", "")
    );
    ( ("opam", [ "list"; "--installed"; "--short" ]),
      (Process.Exited 0, "dune\nocaml-lsp-server\n", "") );
  ]

let test_check_json_stdout_contains_only_json () =
  let run = fake_runner json_cli_responses in
  let code, stdout, stderr =
    capture_output (fun () ->
        Doctor.Cli.run_checks_with ~run ~os:Platform.Linux true)
  in
  let expected_json =
    Doctor.Cli.diagnostics ~run Platform.Linux
    |> Doctor.Report.render_json
  in
  expect_int "json check exit code" 1 code;
  expect_string "json check stderr" "" stderr;
  expect_string "json check stdout" expected_json stdout;
  expect_contains "json stdout starts object" "{\n  \"summary\"" stdout;
  expect_contains "json stdout has diagnostics" "\"diagnostics\"" stdout;
  expect_not_contains "json stdout excludes text header" "OCaml Doctor"
    stdout;
  expect_not_contains "json stdout excludes text statuses" "[WARN]"
    stdout

let missing_switch_tools_responses ~switch_bin =
  [
    (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
    (("opam", [ "var"; "root" ]), (Process.Exited 0, "/tmp/opam\n", ""));
    (("opam", [ "switch"; "show" ]), (Process.Exited 0, "default\n", ""));
    ( ("opam", [ "switch"; "list"; "--short" ]),
      (Process.Exited 0, "default\n", "") );
    ( ("opam", [ "var"; "bin" ]),
      (Process.Exited 0, switch_bin ^ "\n", "") );
    ( ("opam", [ "list"; "--installed"; "--short" ]),
      ( Process.Exited 0,
        "ocaml\ndune\nocaml-lsp-server\nocamlformat\n",
        "" ) );
  ]

let test_windows_opam_env_sync_suggestion_contract () =
  let diagnostics =
    Opam.diagnostics
      ~run:
        (fake_runner
           (missing_switch_tools_responses
              ~switch_bin:"C:\\opam\\default\\bin"))
      Platform.Windows
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "windows env sync severity" Check.Warn env.severity;
  expect_contains "windows env sync detail"
    "Active switch bin: C:\\opam\\default\\bin"
    (expect_some "windows env sync detail" env.detail);
  expect_string "windows env sync suggestion"
    (read_project_text_fixture
       "test/fixtures/opam_env_sync_windows_suggestion.txt")
    (expect_some "windows env sync suggestion" env.suggestion)

let test_unix_opam_env_sync_suggestion_contract () =
  let diagnostics =
    Opam.diagnostics
      ~run:
        (fake_runner
           (missing_switch_tools_responses
              ~switch_bin:"/home/dev/.opam/default/bin"))
      Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "unix env sync severity" Check.Warn env.severity;
  expect_contains "unix env sync detail"
    "Active switch bin: /home/dev/.opam/default/bin"
    (expect_some "unix env sync detail" env.detail);
  expect_string "unix env sync suggestion"
    (read_project_text_fixture
       "test/fixtures/opam_env_sync_unix_suggestion.txt")
    (expect_some "unix env sync suggestion" env.suggestion)

let () =
  List.iter
    (fun test -> test ())
    [
      test_golden_json_for_one_warning;
      test_golden_json_for_error_run;
      test_opam_env_mismatch_reports_active_switch_bin;
      test_emitted_diagnostic_names_are_documented;
      test_check_json_stdout_contains_only_json;
      test_windows_opam_env_sync_suggestion_contract;
      test_unix_opam_env_sync_suggestion_contract;
    ]
