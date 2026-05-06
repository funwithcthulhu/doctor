let diagnostic severity title =
  Doctor.Check.make ~id:title ~title ~suggestion:"opam install ocamlformat"
    severity

let expect_equal label expected actual =
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
    || (index + needle_length <= haystack_length
       && (String.sub haystack index needle_length = needle
          || loop (index + 1)))
  in
  loop 0

let expect_contains label needle haystack =
  if not (contains_substring haystack needle) then
    failwith (Printf.sprintf "%s: missing substring %S" label needle)

let () =
  let ok = diagnostic Doctor.Check.Ok "opam found: 2.2.1" in
  let warn = diagnostic Doctor.Check.Warn "ocamlformat not installed" in
  let error = diagnostic Doctor.Check.Error "opam switch not active" in
  expect_equal "ok exit code" 0 (Doctor.Report.exit_code [ ok ]);
  expect_equal "warning exit code" 1
    (Doctor.Report.exit_code [ ok; warn ]);
  expect_equal "error exit code" 2
    (Doctor.Report.exit_code [ ok; warn; error ]);
  expect_equal "summary ok count" 1
    (let ok_count, _, _ = Doctor.Report.counts [ ok; warn; error ] in
     ok_count);
  let rendered = Doctor.Report.render [ warn ] in
  expect_line "warning line" "[WARN] ocamlformat not installed" rendered;
  expect_line "suggestion line"
    "       Suggested fix: opam install ocamlformat" rendered;
  expect_line "summary line" "Summary: 0 OK, 1 WARN, 0 ERROR" rendered;
  let multiline =
    Doctor.Check.make ~id:"multi" ~title:"multi-line detail"
      ~detail:"first line\nsecond line" ~suggestion:"fix it\ntry again"
      Doctor.Check.Warn
  in
  let rendered = Doctor.Report.render [ multiline ] in
  expect_line "multiline detail first" "       first line" rendered;
  expect_line "multiline detail second" "       second line" rendered;
  expect_line "multiline suggestion first" "       Suggested fix: fix it"
    rendered;
  expect_line "multiline suggestion second" "       try again" rendered;
  let json = Doctor.Report.render_json [ ok; warn; error ] in
  expect_contains "json diagnostics" "\"diagnostics\": [" json;
  expect_contains "json severity" "\"severity\": \"warn\"" json;
  expect_contains "json summary"
    "\"summary\": { \"ok\": 1, \"warn\": 1, \"error\": 1 }" json;
  expect_contains "json exit code" "\"exit_code\": 2" json;
  let escaped =
    Doctor.Check.make ~id:"escape" ~title:"quoted \"title\""
      ~detail:"first line\nsecond line" Doctor.Check.Warn
  in
  let json = Doctor.Report.render_json [ escaped ] in
  expect_contains "json quotes" "\"title\": \"quoted \\\"title\\\"\"" json;
  expect_contains "json newline" "\"detail\": \"first line\\nsecond line\""
    json;
  let empty_json =
    String.concat "\n"
      [
        "{";
        "  \"diagnostics\": [],";
        "  \"summary\": { \"ok\": 0, \"warn\": 0, \"error\": 0 },";
        "  \"exit_code\": 0";
        "}";
      ]
    ^ "\n"
  in
  expect_string "empty json" empty_json (Doctor.Report.render_json [])
