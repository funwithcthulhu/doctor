type severity = Ok | Warn | Error

type diagnostic = {
  id : string;
  title : string;
  severity : severity;
  detail : string option;
  suggestion : string option;
}

let make ?detail ?suggestion ~id ~title severity =
  { id; title; severity; detail; suggestion }

let severity_to_string = function
  | Ok -> "OK"
  | Warn -> "WARN"
  | Error -> "ERROR"

let max_severity left right =
  match (left, right) with
  | Error, _ | _, Error -> Error
  | Warn, _ | _, Warn -> Warn
  | Ok, Ok -> Ok

let aggregate diagnostics =
  List.fold_left
    (fun severity diagnostic -> max_severity severity diagnostic.severity)
    Ok diagnostics

let exit_code diagnostics =
  match aggregate diagnostics with Ok -> 0 | Warn -> 1 | Error -> 2

let clean_output output =
  output |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let first_output_line (result : Process.result) =
  match clean_output result.Process.stdout with
  | line :: _ -> Some line
  | [] -> (
      match clean_output result.Process.stderr with
      | line :: _ -> Some line
      | [] -> None)

let parse_ocaml_version line =
  match String.split_on_char ',' line with
  | [ _prefix; suffix ] -> (
      match String.split_on_char ' ' (String.trim suffix) with
      | [ "version"; version ] -> version
      | _ -> String.trim suffix)
  | _ -> String.trim line

type command_check = {
  command : string;
  args : string list;
  label : string;
  missing_severity : severity;
  missing_suggestion : string;
  version_parser : string -> string;
}

let title_with_version label version =
  match version with
  | Some version when version <> "" ->
      Printf.sprintf "%s found: %s" label version
  | _ -> Printf.sprintf "%s found" label

let command_version result parser =
  result |> first_output_line |> Option.map parser

let command_diagnostic ~(run : Process.runner) spec =
  let result = run spec.command spec.args in
  match result.status with
  | Process.Exited 0 ->
      let version = command_version result spec.version_parser in
      let title = title_with_version spec.label version in
      make ~id:("command." ^ spec.command) ~title Ok
  | Process.Spawn_error _ ->
      make
        ~id:("command." ^ spec.command)
        ~title:(Printf.sprintf "%s not found" spec.label)
        ~detail:
          (Printf.sprintf "The `%s` command is not available on PATH."
             spec.command)
        ~suggestion:spec.missing_suggestion spec.missing_severity
  | Exited _ | Signaled _ | Stopped _ ->
      make
        ~id:("command." ^ spec.command)
        ~title:(Printf.sprintf "%s command failed" spec.label)
        ~detail:(Process.summary result) ~suggestion:spec.missing_suggestion
        spec.missing_severity

let lsp_command_diagnostic ~(run : Process.runner) =
  let primary = run "ocaml-lsp-server" [ "--version" ] in
  match primary.status with
  | Process.Exited 0 ->
      let version = command_version primary String.trim in
      let title = title_with_version "OCaml LSP" version in
      make ~id:"command.ocaml-lsp-server" ~title Ok
  | Process.Spawn_error _ -> (
      let fallback = run "ocamllsp" [ "--version" ] in
      match fallback.status with
      | Process.Exited 0 ->
          let version = command_version fallback String.trim in
          let title =
            match version with
            | Some version when version <> "" ->
                Printf.sprintf "OCaml LSP found: %s (ocamllsp)" version
            | _ -> "OCaml LSP found (ocamllsp)"
          in
          make ~id:"command.ocaml-lsp-server" ~title Ok
      | Process.Spawn_error _ ->
          make ~id:"command.ocaml-lsp-server"
            ~title:"OCaml LSP command not found"
            ~detail:
              "Checked `ocaml-lsp-server` and `ocamllsp`; neither command is \
               available on PATH."
            ~suggestion:"opam install ocaml-lsp-server" Warn
      | _ ->
          make ~id:"command.ocaml-lsp-server"
            ~title:"OCaml LSP found, but its version could not be read"
            ~detail:(Process.summary fallback)
            ~suggestion:
              "Try running `ocamllsp --version` directly, or reinstall it with \
               opam."
            Warn)
  | _ ->
      make ~id:"command.ocaml-lsp-server"
        ~title:"ocaml-lsp-server found, but its version could not be read"
        ~detail:(Process.summary primary)
        ~suggestion:
          "Try running `ocaml-lsp-server --version` directly, or reinstall it \
           with opam."
        Warn

let core_command_specs =
  [
    {
      command = "opam";
      args = [ "--version" ];
      label = "opam";
      missing_severity = Error;
      missing_suggestion =
        "Install opam from https://opam.ocaml.org/doc/Install.html";
      version_parser = String.trim;
    };
    {
      command = "ocaml";
      args = [ "-version" ];
      label = "OCaml";
      missing_severity = Error;
      missing_suggestion =
        "Create or select an opam switch, then sync your shell environment.";
      version_parser = parse_ocaml_version;
    };
    {
      command = "dune";
      args = [ "--version" ];
      label = "dune";
      missing_severity = Warn;
      missing_suggestion = "opam install dune";
      version_parser = String.trim;
    };
  ]

let ocamlformat_spec =
  {
    command = "ocamlformat";
    args = [ "--version" ];
    label = "ocamlformat";
    missing_severity = Warn;
    missing_suggestion = "opam install ocamlformat";
    version_parser = String.trim;
  }

let command_diagnostics ~run =
  List.map (command_diagnostic ~run) core_command_specs
  @ [ lsp_command_diagnostic ~run; command_diagnostic ~run ocamlformat_spec ]
