let expect_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_status label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: unexpected process status" label)

let expect_nonzero label = function
  | Doctor.Process.Exited 0 ->
      failwith (Printf.sprintf "%s: expected non-zero exit" label)
  | _ -> ()

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

let normalize_newlines text =
  text |> String.split_on_char '\r' |> String.concat ""

let built_exe name =
  [
    Filename.concat ".." (Filename.concat "bin" (name ^ ".exe"));
    Filename.concat "_build"
      (Filename.concat "default"
         (Filename.concat "bin" (name ^ ".exe")));
    Filename.concat "bin" (name ^ ".exe");
  ]
  |> List.find_opt Sys.file_exists
  |> function
  | Some path -> path
  | None -> failwith (Printf.sprintf "%s executable not found" name)

let run_doctor args =
  let exe = built_exe "main" in
  Doctor.Process.run exe args

let run_opam_doctor args =
  let exe = built_exe "opam_doctor" in
  Doctor.Process.run exe args

let test_version_display_matches_current () =
  expect_equal "version display"
    ("doctor " ^ Doctor.Version.current)
    Doctor.Version.display

let test_version_command_prints_current_version () =
  let result = run_doctor [ "version" ] in
  expect_status "version command status" (Doctor.Process.Exited 0)
    result.status;
  expect_equal "version command output"
    (Doctor.Version.display ^ "\n")
    (normalize_newlines result.stdout)

let test_help_exits_successfully () =
  let result = run_doctor [ "--help=plain" ] in
  expect_status "help command status" (Doctor.Process.Exited 0)
    result.status;
  expect_contains "help output"
    "Run OCaml development environment diagnostics" result.stdout

let test_check_help_mentions_json () =
  let result = run_doctor [ "check"; "--help=plain" ] in
  expect_status "check help command status" (Doctor.Process.Exited 0)
    result.status;
  expect_contains "check help output mentions json" "--json"
    result.stdout

let test_opam_doctor_help_uses_plugin_binary_name () =
  let result = run_opam_doctor [ "--help=plain" ] in
  expect_status "opam-doctor help status" (Doctor.Process.Exited 0)
    result.status;
  expect_contains "opam-doctor help name" "opam-doctor" result.stdout;
  expect_contains "opam-doctor help command" "check" result.stdout

let test_opam_doctor_version_matches_doctor () =
  let result = run_opam_doctor [ "version" ] in
  expect_status "opam-doctor version status" (Doctor.Process.Exited 0)
    result.status;
  expect_equal "opam-doctor version output"
    (Doctor.Version.display ^ "\n")
    (normalize_newlines result.stdout)

let test_invalid_command_exits_nonzero () =
  let result = run_doctor [ "not-a-command" ] in
  expect_nonzero "invalid command status" result.status

let test_missing_command_exits_nonzero () =
  let result = run_doctor [] in
  expect_nonzero "missing command status" result.status;
  expect_contains "missing command stderr"
    "required COMMAND name is missing" result.stderr

let () =
  List.iter
    (fun test -> test ())
    [
      test_version_display_matches_current;
      test_version_command_prints_current_version;
      test_help_exits_successfully;
      test_check_help_mentions_json;
      test_opam_doctor_help_uses_plugin_binary_name;
      test_opam_doctor_version_matches_doctor;
      test_invalid_command_exits_nonzero;
      test_missing_command_exits_nonzero;
    ]
