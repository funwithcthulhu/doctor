let version = Doctor.Version.current

type output_format = Text | Json

let render_diagnostics format diagnostics =
  match format with
  | Text -> Doctor.Report.render diagnostics
  | Json -> Doctor.Report.render_json diagnostics

let run_checks output_format =
  try
    let run = Doctor.Process.run in
    let os = Doctor.Platform.detect ~run () in
    let diagnostics =
      [ Doctor.Platform.diagnostic os ]
      @ Doctor.Check.command_diagnostics ~run
      @ Doctor.Opam.diagnostics ~run os
      @ Doctor.Editor.diagnostics ~run
    in
    print_string (render_diagnostics output_format diagnostics);
    Doctor.Report.exit_code diagnostics
  with exn ->
    prerr_endline ("doctor internal failure: " ^ Printexc.to_string exn);
    3

let print_version () =
  print_endline Doctor.Version.display;
  0

open Cmdliner

let output_format =
  let formats = [ ("text", Text); ("json", Json) ] in
  let doc =
    "Choose the output format. $(docv) must be $(b,text) or $(b,json)."
  in
  Arg.(value & opt (enum formats) Text & info [ "format" ] ~docv:"FORMAT" ~doc)

let exit_infos =
  [
    Cmd.Exit.info ~doc:"no warnings or errors." 0;
    Cmd.Exit.info ~doc:"warnings only." 1;
    Cmd.Exit.info ~doc:"one or more errors." 2;
    Cmd.Exit.info ~doc:"unexpected internal failure." 3;
  ]
  @ List.filter (fun info -> Cmd.Exit.info_code info <> 0) Cmd.Exit.defaults

let check_cmd =
  let doc = "Run OCaml development environment diagnostics." in
  Cmd.v
    (Cmd.info "check" ~doc ~exits:exit_infos)
    Term.(const run_checks $ output_format)

let version_cmd =
  let doc = "Print the doctor version." in
  Cmd.v (Cmd.info "version" ~doc) Term.(const print_version $ const ())

let default_cmd =
  let doc =
    "Inspect an OCaml development environment and print actionable diagnostics."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "doctor checks for common OCaml, opam, dune, LSP, formatter, shell \
         environment, and VS Code setup issues. It does not modify your \
         machine.";
    ]
  in
  Cmd.group
    (Cmd.info "doctor" ~version ~doc ~man ~exits:exit_infos)
    [ check_cmd; version_cmd ]

let () =
  match Cmd.eval_value' default_cmd with
  | `Ok code -> exit code
  | `Exit code -> exit code
