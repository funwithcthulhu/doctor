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

let doctor_exe () =
  [
    Filename.concat ".." (Filename.concat "bin" "main.exe");
    Filename.concat "_build"
      (Filename.concat "default" (Filename.concat "bin" "main.exe"));
    Filename.concat "bin" "main.exe";
  ]
  |> List.find_opt Sys.file_exists
  |> function
  | Some path -> path
  | None -> failwith "doctor executable not found"

let run_doctor args =
  let exe = doctor_exe () in
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
  let result = run_doctor [ "--help" ] in
  expect_status "help command status" (Doctor.Process.Exited 0)
    result.status;
  expect_contains "help output"
    "Run OCaml development environment diagnostics" result.stdout;
  expect_contains "help output mentions json" "--json" result.stdout

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
      test_invalid_command_exits_nonzero;
      test_missing_command_exits_nonzero;
    ]
