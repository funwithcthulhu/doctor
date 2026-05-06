let indent_for status = String.make (String.length status + 3) ' '

let non_empty_lines text =
  text |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let format_extra_lines indent ~prefix text =
  match non_empty_lines text with
  | [] -> []
  | first :: rest ->
      (indent ^ prefix ^ first) :: List.map (fun line -> indent ^ line) rest

let format_diagnostic diagnostic =
  let status = Check.severity_to_string diagnostic.Check.severity in
  let first_line = Printf.sprintf "[%s] %s" status diagnostic.Check.title in
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
