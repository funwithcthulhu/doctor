let indent_for status =
  String.make (String.length status + 3) ' '

let non_empty_lines text =
  text
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let format_extra_lines indent ~prefix text =
  match non_empty_lines text with
  | [] -> []
  | first :: rest ->
      (indent ^ prefix ^ first)
      :: List.map (fun line -> indent ^ line) rest

let format_diagnostic diagnostic =
  let status = Check.severity_to_string diagnostic.Check.severity in
  let first_line =
    Printf.sprintf "[%s] %s" status diagnostic.Check.title
  in
  let indent = indent_for status in
  let extra =
    (match diagnostic.detail with
    | Some detail -> format_extra_lines indent ~prefix:"" detail
    | None -> [])
    @
    match diagnostic.suggestion with
    | Some suggestion ->
        format_extra_lines indent ~prefix:"Suggested fix: " suggestion
    | None -> []
  in
  String.concat "\n" (first_line :: extra)

let counts diagnostics =
  List.fold_left
    (fun (ok, warn, error) diagnostic ->
      match diagnostic.Check.severity with
      | Ok -> (ok + 1, warn, error)
      | Warn -> (ok, warn + 1, error)
      | Error -> (ok, warn, error + 1))
    (0, 0, 0) diagnostics

let format_summary diagnostics =
  let ok, warn, error = counts diagnostics in
  Printf.sprintf "Summary: %d OK, %d WARN, %d ERROR" ok warn error

let json_escape text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\012' -> Buffer.add_string buffer "\\f"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | char when Char.code char < 0x20 ->
          Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code char))
      | char -> Buffer.add_char buffer char)
    text;
  Buffer.contents buffer

let json_string text =
  Printf.sprintf "\"%s\"" (json_escape text)

let json_option = function
  | Some value -> json_string value
  | None -> "null"

let json_severity = function
  | Check.Ok -> "ok"
  | Check.Warn -> "warn"
  | Check.Error -> "error"

let render_json_diagnostic diagnostic =
  String.concat "\n"
    [
      "    {";
      Printf.sprintf "      \"id\": %s," (json_string diagnostic.Check.id);
      Printf.sprintf "      \"severity\": %s,"
        (json_string (json_severity diagnostic.severity));
      Printf.sprintf "      \"title\": %s," (json_string diagnostic.title);
      Printf.sprintf "      \"detail\": %s," (json_option diagnostic.detail);
      Printf.sprintf "      \"suggestion\": %s"
        (json_option diagnostic.suggestion);
      "    }";
    ]

let render_json diagnostics =
  let ok, warn, error = counts diagnostics in
  let diagnostic_lines =
    diagnostics |> List.map render_json_diagnostic |> String.concat ",\n"
  in
  let diagnostics_json =
    match diagnostic_lines with
    | "" -> "[]"
    | _ -> "[\n" ^ diagnostic_lines ^ "\n  ]"
  in
  String.concat "\n"
    [
      "{";
      Printf.sprintf "  \"diagnostics\": %s," diagnostics_json;
      Printf.sprintf
        "  \"summary\": { \"ok\": %d, \"warn\": %d, \"error\": %d }," ok warn
        error;
      Printf.sprintf "  \"exit_code\": %d" (Check.exit_code diagnostics);
      "}";
    ]
  ^ "\n"

let render diagnostics =
  let body =
    match diagnostics with
    | [] -> "No diagnostics."
    | _ ->
        (diagnostics |> List.map format_diagnostic |> String.concat "\n")
        ^ "\n\n" ^ format_summary diagnostics
  in
  "OCaml Doctor\n\n" ^ body ^ "\n"

let exit_code = Check.exit_code
