let diagnostic ?detail ?suggestion severity title =
  Doctor.Check.make ?detail ?suggestion ~id:title ~title severity

let expect_int label expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let expect_string label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_line label needle haystack =
  if
    not
      (List.exists (String.equal needle)
         (String.split_on_char '\n' haystack))
  then failwith (Printf.sprintf "%s: missing line %S" label needle)

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

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let normalize_newlines text =
  text |> String.split_on_char '\r' |> String.concat ""

let find_fixture name =
  let candidates =
    [
      Filename.concat "fixtures" name;
      Filename.concat (Filename.concat "test" "fixtures") name;
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith (Printf.sprintf "fixture %s not found" name)

let read_fixture name =
  read_file (find_fixture name) |> normalize_newlines

let ok = diagnostic Doctor.Check.Ok "opam found: 2.2.1"

let warn =
  diagnostic Doctor.Check.Warn "ocamlformat not installed"
    ~suggestion:"opam install ocamlformat"

let error = diagnostic Doctor.Check.Error "opam switch not active"

let golden_all_ok =
  [
    Doctor.Check.make ~id:"platform.os"
      ~title:"platform detected: Linux" Doctor.Check.Ok;
    Doctor.Check.make ~id:"command.opam" ~title:"opam found: 2.2.1"
      Doctor.Check.Ok;
  ]

let golden_warn =
  [
    Doctor.Check.make ~id:"command.opam" ~title:"opam found: 2.2.1"
      Doctor.Check.Ok;
    Doctor.Check.make ~id:"command.ocamlformat"
      ~title:"ocamlformat not installed"
      ~detail:"The `ocamlformat` command is not available on PATH."
      ~suggestion:"opam install ocamlformat" Doctor.Check.Warn;
  ]

let golden_error =
  [
    Doctor.Check.make ~id:"opam.switch.active"
      ~title:"opam switch not active"
      ~detail:
        "opam switch show returned exit 1\n\
         stderr: No switch is currently set"
      ~suggestion:"eval $(opam env)" Doctor.Check.Error;
  ]

let test_exit_codes_and_counts () =
  expect_int "ok exit code" 0 (Doctor.Report.exit_code [ ok ]);
  expect_int "warning exit code" 1
    (Doctor.Report.exit_code [ ok; warn ]);
  expect_int "error exit code" 2
    (Doctor.Report.exit_code [ ok; warn; error ]);
  expect_int "summary ok count" 1
    (let ok_count, _, _ = Doctor.Report.counts [ ok; warn; error ] in
     ok_count)

let test_text_report_includes_suggestions () =
  let rendered = Doctor.Report.render [ warn ] in
  expect_line "warning line" "[WARN] ocamlformat not installed" rendered;
  expect_line "suggestion line"
    "       Suggested fix: opam install ocamlformat" rendered;
  expect_line "summary line" "Summary: 0 OK, 1 WARN, 0 ERROR" rendered

let test_multiline_detail_and_suggestion_are_indented () =
  let diagnostic =
    diagnostic Doctor.Check.Warn "multi-line detail"
      ~detail:"first line\nsecond line" ~suggestion:"fix it\ntry again"
  in
  let rendered = Doctor.Report.render [ diagnostic ] in
  expect_line "multiline detail first" "       first line" rendered;
  expect_line "multiline detail second" "       second line" rendered;
  expect_line "multiline suggestion first"
    "       Suggested fix: fix it" rendered;
  expect_line "multiline suggestion second" "       try again" rendered

let test_json_report_contains_diagnostics_summary_and_exit_code () =
  let json =
    Doctor.Report.render_json
      [
        ok;
        Doctor.Check.make ~id:"command.ocamlformat"
          ~title:"ocamlformat not installed"
          ~detail:"The `ocamlformat` command is not available on PATH."
          ~suggestion:"opam install ocamlformat" Doctor.Check.Warn;
        error;
      ]
  in
  expect_line "json status" "    \"status\": \"error\"," json;
  expect_line "json exit code" "    \"exit_code\": 2" json;
  expect_contains "json diagnostic name"
    "\"name\": \"command.ocamlformat\"" json;
  expect_contains "json diagnostic details"
    "\"details\": [\"The `ocamlformat` command is not available on \
     PATH.\", \"Suggested fix: opam install ocamlformat\"]"
    json

let test_json_escapes_strings () =
  let diagnostic =
    diagnostic Doctor.Check.Warn "quoted \"message\" with \\ path"
      ~detail:"first line\nsecond\tline"
  in
  let json = Doctor.Report.render_json [ diagnostic ] in
  expect_contains "json quotes"
    "\"message\": \"quoted \\\"message\\\" with \\\\ path\"" json;
  expect_contains "json newline and tab"
    "\"details\": [\"first line\", \"second\\tline\"]" json

let test_json_represents_warning_and_error_separately () =
  let json = Doctor.Report.render_json [ warn; error ] in
  expect_contains "json warning status" "\"status\": \"warn\"" json;
  expect_contains "json error status" "\"status\": \"error\"" json;
  expect_contains "json error summary" "\"status\": \"error\"," json;
  expect_contains "json error exit code" "\"exit_code\": 2" json

let test_empty_json_report () =
  let expected =
    String.concat "\n"
      [
        "{";
        "  \"summary\": {";
        "    \"status\": \"ok\",";
        "    \"exit_code\": 0";
        "  },";
        "  \"diagnostics\": []";
        "}";
      ]
    ^ "\n"
  in
  expect_string "empty json" expected (Doctor.Report.render_json [])

let test_golden_text_reports () =
  expect_string "all-ok golden report"
    (read_fixture "report_all_ok.txt")
    (Doctor.Report.render golden_all_ok);
  expect_string "warn golden report"
    (read_fixture "report_warn.txt")
    (Doctor.Report.render golden_warn);
  expect_string "error golden report"
    (read_fixture "report_error.txt")
    (Doctor.Report.render golden_error)

let () =
  List.iter
    (fun test -> test ())
    [
      test_exit_codes_and_counts;
      test_text_report_includes_suggestions;
      test_multiline_detail_and_suggestion_are_indented;
      test_json_report_contains_diagnostics_summary_and_exit_code;
      test_json_escapes_strings;
      test_json_represents_warning_and_error_separately;
      test_empty_json_report;
      test_golden_text_reports;
    ]
