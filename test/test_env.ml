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

let test_path_with_dot_warns () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:(fake_env [ ("PATH", "/usr/bin:.:/bin") ])
  in
  let diagnostic = expect_one_diagnostic "dot path" diagnostics in
  expect_string "dot path id" "env.path.current-directory" diagnostic.id;
  expect_severity "dot path severity" Doctor.Check.Warn
    diagnostic.severity;
  expect_string "dot path title" "PATH includes the current directory"
    diagnostic.title;
  expect_contains "dot path detail" "Current-directory PATH entries: ."
    (expect_some "dot path detail" diagnostic.detail)

let test_empty_unix_path_segment_warns () =
  let diagnostics =
    Env.diagnostics Platform.Linux
      ~env:(fake_env [ ("PATH", "/usr/bin::/bin:") ])
  in
  let diagnostic =
    expect_one_diagnostic "empty path segment" diagnostics
  in
  expect_string "empty path segment id" "env.path.current-directory"
    diagnostic.id;
  expect_contains "empty path suggestion" "Remove `.`"
    (expect_some "empty path suggestion" diagnostic.suggestion)

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

let () =
  List.iter
    (fun test -> test ())
    [
      test_path_with_dot_warns;
      test_empty_unix_path_segment_warns;
      test_normal_path_is_quiet;
      test_missing_path_is_quiet;
    ]
