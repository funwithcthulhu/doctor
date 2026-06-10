module Check = Doctor.Check
module Env = Doctor.Env
module Platform = Doctor.Platform

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

let fake_env values name = List.assoc_opt name values

let expect_no_diagnostics label diagnostics =
  expect_int label 0 (List.length diagnostics)

let expect_one_diagnostic label = function
  | [ diagnostic ] -> diagnostic
  | diagnostics ->
      failwith
        (Printf.sprintf "%s: expected one diagnostic, got %d" label
           (List.length diagnostics))

let find_diagnostic id diagnostics =
  diagnostics
  |> List.find_opt (fun diagnostic ->
      String.equal diagnostic.Check.id id)
  |> expect_some ("diagnostic " ^ id)

let test_path_with_dot_warns () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:(fake_env [ ("PATH", "/usr/bin:.:/bin") ])
  in
  let diagnostic = expect_one_diagnostic "dot path" diagnostics in
  expect_string "dot path id" "env.path.current-directory"
    diagnostic.Check.id;
  expect_severity "dot path severity" Check.Warn
    diagnostic.Check.severity;
  expect_string "dot path title" "PATH includes the current directory"
    diagnostic.Check.title;
  expect_contains "dot path detail" "Current-directory PATH entries: ."
    (expect_some "dot path detail" diagnostic.Check.detail)

let test_empty_unix_path_segment_warns () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:(fake_env [ ("PATH", "/usr/bin::/bin:") ])
  in
  let diagnostic =
    expect_one_diagnostic "empty path segment" diagnostics
  in
  expect_string "empty path segment id" "env.path.current-directory"
    diagnostic.Check.id;
  expect_contains "empty path suggestion" "Remove `.`"
    (expect_some "empty path suggestion" diagnostic.Check.suggestion)

let test_normal_path_is_quiet () =
  Env.diagnostics Platform.Linux
    ~env:(fake_env [ ("PATH", "/usr/bin:/bin") ])
  |> expect_no_diagnostics "normal path";
  Env.diagnostics Platform.Windows
    ~env:(fake_env [ ("PATH", "C:\\Windows\\System32;C:\\opam\\bin") ])
  |> expect_no_diagnostics "normal windows path"

let test_missing_path_is_quiet () =
  Env.diagnostics Platform.Linux ~env:(fake_env [])
  |> expect_no_diagnostics "missing path"

let test_forced_color_variables_warn () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:
        (fake_env
           [
             ("PATH", "/usr/bin:/bin");
             ("CLICOLOR_FORCE", "1");
             ("CLICOLOR_FORCED", "yes");
           ])
  in
  let diagnostic = expect_one_diagnostic "forced color" diagnostics in
  expect_string "forced color id" "env.color.forced" diagnostic.Check.id;
  expect_severity "forced color severity" Check.Warn
    diagnostic.Check.severity;
  expect_string "forced color title" "forced color output is enabled"
    diagnostic.Check.title;
  let detail =
    expect_some "forced color detail" diagnostic.Check.detail
  in
  expect_contains "forced color CLICOLOR_FORCE" "CLICOLOR_FORCE=1"
    detail;
  expect_contains "forced color CLICOLOR_FORCED" "CLICOLOR_FORCED=yes"
    detail

let test_normal_clicolor_is_quiet () =
  Env.diagnostics Platform.Linux
    ~env:
      (fake_env
         [
           ("PATH", "/usr/bin:/bin");
           ("CLICOLOR", "1");
           ("CLICOLOR_FORCE", "0");
           ("CLICOLOR_FORCED", "false");
         ])
  |> expect_no_diagnostics "normal clicolor"

let test_forced_clicolor_value_warns () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:
        (fake_env [ ("PATH", "/usr/bin:/bin"); ("CLICOLOR", "always") ])
  in
  let diagnostic =
    expect_one_diagnostic "forced clicolor" diagnostics
  in
  expect_string "forced clicolor id" "env.color.forced"
    diagnostic.Check.id;
  expect_contains "forced clicolor detail" "CLICOLOR=always"
    (expect_some "forced clicolor detail" diagnostic.Check.detail)

let test_grep_options_warns () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:
        (fake_env
           [
             ("PATH", "/usr/bin:/bin");
             ("GREP_OPTIONS", "--color=always");
           ])
  in
  let diagnostic = expect_one_diagnostic "grep options" diagnostics in
  expect_string "grep options id" "env.grep-options" diagnostic.Check.id;
  expect_severity "grep options severity" Check.Warn
    diagnostic.Check.severity;
  expect_string "grep options title" "GREP_OPTIONS is set"
    diagnostic.Check.title;
  expect_string "grep options detail" "GREP_OPTIONS=--color=always"
    (expect_some "grep options detail" diagnostic.Check.detail)

let test_multiple_environment_hygiene_warnings_can_be_reported () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:
        (fake_env
           [
             ("PATH", "/usr/bin:.:/bin");
             ("CLICOLOR_FORCE", "1");
             ("GREP_OPTIONS", "-R");
           ])
  in
  expect_int "multiple env warnings" 3 (List.length diagnostics);
  ignore (find_diagnostic "env.path.current-directory" diagnostics);
  ignore (find_diagnostic "env.color.forced" diagnostics);
  ignore (find_diagnostic "env.grep-options" diagnostics)

let () =
  List.iter
    (fun test -> test ())
    [
      test_path_with_dot_warns;
      test_empty_unix_path_segment_warns;
      test_normal_path_is_quiet;
      test_missing_path_is_quiet;
      test_forced_color_variables_warn;
      test_normal_clicolor_is_quiet;
      test_forced_clicolor_value_warns;
      test_grep_options_warns;
      test_multiple_environment_hygiene_warnings_can_be_reported;
    ]
