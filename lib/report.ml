let indent_for status = String.make (String.length status + 3) ' '

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
          Buffer.add_string buffer
            (Printf.sprintf "\\u%04x" (Char.code char))
      | char -> Buffer.add_char buffer char)
    text;
  Buffer.contents buffer

let json_string text = Printf.sprintf "\"%s\"" (json_escape text)

let json_status = function
  | Check.Ok -> "ok"
  | Check.Warn -> "warn"
  | Check.Error -> "error"

let option_lines = function
  | Some text -> non_empty_lines text
  | None -> []

let suggestion_lines = function
  | Some text -> (
      match non_empty_lines text with
      | [] -> []
      | first :: rest -> ("Suggested fix: " ^ first) :: rest)
  | None -> []

let diagnostic_details diagnostic =
  option_lines diagnostic.Check.detail
  @ suggestion_lines diagnostic.Check.suggestion

let json_array values =
  match values with
  | [] -> "[]"
  | _ ->
      values |> List.map json_string |> String.concat ", "
      |> Printf.sprintf "[%s]"

let render_json_diagnostic diagnostic =
  let field ?(comma = true) name value =
    Printf.sprintf "      \"%s\": %s%s" name value
      (if comma then "," else "")
  in
  String.concat "\n"
    [
      "    {";
      field "name" (json_string diagnostic.Check.id);
      field "status"
        (json_string (json_status diagnostic.Check.severity));
      field "message" (json_string diagnostic.Check.title);
      field ~comma:false "details"
        (json_array (diagnostic_details diagnostic));
      "    }";
    ]

let render_json diagnostics =
  let status = Check.aggregate diagnostics |> json_status in
  let exit_code = Check.exit_code diagnostics in
  let diagnostic_lines =
    diagnostics
    |> List.map render_json_diagnostic
    |> String.concat ",\n"
  in
  let diagnostics_json =
    match diagnostic_lines with
    | "" -> "[]"
    | _ -> "[\n" ^ diagnostic_lines ^ "\n  ]"
  in
  String.concat "\n"
    [
      "{";
      "  \"summary\": {";
      Printf.sprintf "    \"status\": %s," (json_string status);
      Printf.sprintf "    \"exit_code\": %d" exit_code;
      "  },";
      Printf.sprintf "  \"diagnostics\": %s" diagnostics_json;
      "}";
    ]
  ^ "\n"

let render diagnostics =
  let body =
    match diagnostics with
    | [] -> "No diagnostics."
    | _ ->
        (diagnostics |> List.map format_diagnostic |> String.concat "\n")
        ^ "\n\n"
        ^ format_summary diagnostics
  in
  "OCaml Doctor\n\n" ^ body ^ "\n"

let exit_code = Check.exit_code
