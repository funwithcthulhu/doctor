let non_empty_lines output =
  output |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let first_stdout_line result =
  match result.Process.status with
  | Process.Exited 0 -> (
      match non_empty_lines result.stdout with
      | line :: _ -> Some line
      | [] -> None)
  | _ -> None

let parse_active_switch output =
  match non_empty_lines output with
  | [] -> None
  | line :: _ ->
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"[error]" lower then None else Some line

let trim_switch_marker line =
  let line = String.trim line in
  match line with
  | "" -> ""
  | _ when line.[0] = '*' ->
      String.trim (String.sub line 1 (String.length line - 1))
  | _ -> line

let parse_switch_list output =
  non_empty_lines output
  |> List.map trim_switch_marker
  |> List.filter (fun line -> line <> "")

let words line =
  line
  |> String.map (fun c -> if Process.is_whitespace c then ' ' else c)
  |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")

let parse_installed_packages output =
  non_empty_lines output
  |> List.map (fun line ->
      match words line with package :: _ -> package | [] -> line)

let has_package packages package = List.exists (String.equal package) packages

let opam_available ~(run : Process.runner) =
  match (run "opam" [ "--version" ]).status with
  | Process.Exited 0 -> true
  | _ -> false

let switch_suggestion os switches =
  match switches with
  | [] -> "opam switch create 5.2.0"
  | _ :: _ -> Platform.environment_sync_suggestion os

let initialized_diagnostic ~(run : Process.runner) =
  let result = run "opam" [ "var"; "root" ] in
  match result.status with
  | Process.Exited 0 -> (
      match first_stdout_line result with
      | Some root ->
          Check.make ~id:"opam.initialized" ~title:"opam initialized"
            ~detail:(Printf.sprintf "Root: %s" root)
            Check.Ok
      | None ->
          Check.make ~id:"opam.initialized" ~title:"opam root could not be read"
            ~detail:(Process.summary result) ~suggestion:"opam init" Check.Warn)
  | _ ->
      Check.make ~id:"opam.initialized"
        ~title:"opam does not appear initialized"
        ~detail:(Process.summary result) ~suggestion:"opam init" Check.Warn

let switch_diagnostics ~(run : Process.runner) os =
  let show = run "opam" [ "switch"; "show" ] in
  let switches = run "opam" [ "switch"; "list"; "--short" ] in
  let switch_list =
    match switches.status with
    | Process.Exited 0 -> parse_switch_list switches.stdout
    | _ -> []
  in
  let show_diagnostic =
    match show.status with
    | Process.Exited 0 -> (
        match parse_active_switch show.stdout with
        | Some active ->
            Check.make ~id:"opam.switch.active"
              ~title:(Printf.sprintf "active switch: %s" active)
              Check.Ok
        | None ->
            let suggestion = switch_suggestion os switch_list in
            Check.make ~id:"opam.switch.active" ~title:"opam switch not active"
              ~detail:"opam did not report an active switch." ~suggestion
              Check.Error)
    | _ ->
        let suggestion = switch_suggestion os switch_list in
        Check.make ~id:"opam.switch.active" ~title:"opam switch not active"
          ~detail:(Process.summary show) ~suggestion Check.Error
  in
  let list_diagnostic =
    match switches.status with
    | Process.Exited 0 ->
        let count = List.length switch_list in
        let detail =
          match switch_list with
          | [] -> None
          | _ :: _ -> Some (String.concat ", " switch_list)
        in
        Check.make ~id:"opam.switch.list"
          ~title:(Printf.sprintf "opam switches available: %d" count)
          ?detail Check.Ok
    | _ ->
        Check.make ~id:"opam.switch.list" ~title:"could not list opam switches"
          ~detail:(Process.summary switches)
          ~suggestion:"Run `opam switch list` to inspect your switches."
          Check.Warn
  in
  [ show_diagnostic; list_diagnostic ]

let locate_ocaml ~(run : Process.runner) os =
  let locator, args_for = Platform.command_locator os in
  first_stdout_line (run locator (args_for "ocaml"))

let switch_bin_diagnostic ~(run : Process.runner) os =
  let bin = run "opam" [ "var"; "bin" ] in
  match first_stdout_line bin with
  | None -> []
  | Some switch_bin -> (
      match locate_ocaml ~run os with
      | Some path when Platform.is_path_under ~parent:switch_bin path ->
          [
            Check.make ~id:"opam.env.sync"
              ~title:"shell environment appears synced with opam"
              ~detail:(Printf.sprintf "ocaml resolves to %s" path)
              Check.Ok;
          ]
      | Some path ->
          [
            Check.make ~id:"opam.env.sync"
              ~title:"shell environment may be out of sync with opam"
              ~detail:
                (Printf.sprintf
                   "ocaml resolves to %s, but the active switch bin is %s." path
                   switch_bin)
              ~suggestion:(Platform.environment_sync_suggestion os)
              Check.Warn;
          ]
      | None -> [])

let package_diagnostic packages package ~optional =
  if has_package packages package then
    Check.make
      ~id:("opam.package." ^ package)
      ~title:(Printf.sprintf "%s package installed" package)
      Check.Ok
  else
    let title =
      if optional then Printf.sprintf "%s not installed (optional)" package
      else Printf.sprintf "%s not installed" package
    in
    Check.make
      ~id:("opam.package." ^ package)
      ~title
      ~suggestion:(Printf.sprintf "opam install %s" package)
      Check.Warn

let package_diagnostics ~(run : Process.runner) =
  let result = run "opam" [ "list"; "--installed"; "--short" ] in
  match result.status with
  | Process.Exited 0 ->
      let packages = parse_installed_packages result.stdout in
      [
        package_diagnostic packages "dune" ~optional:false;
        package_diagnostic packages "ocaml-lsp-server" ~optional:false;
        package_diagnostic packages "ocamlformat" ~optional:false;
        package_diagnostic packages "utop" ~optional:true;
      ]
  | _ ->
      [
        Check.make ~id:"opam.packages"
          ~title:"could not read installed opam packages"
          ~detail:(Process.summary result)
          ~suggestion:"Run `opam list --installed --short` to inspect packages."
          Check.Warn;
      ]

let diagnostics ~(run : Process.runner) os =
  if opam_available ~run then
    [ initialized_diagnostic ~run ]
    @ switch_diagnostics ~run os
    @ switch_bin_diagnostic ~run os
    @ package_diagnostics ~run
  else
    [
      Check.make ~id:"opam.initialized"
        ~title:"opam checks skipped because opam is missing"
        ~suggestion:"Install opam from https://opam.ocaml.org/doc/Install.html"
        Check.Error;
    ]
