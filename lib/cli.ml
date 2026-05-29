let render_diagnostics ~json diagnostics =
  if json then Report.render_json diagnostics
  else Report.render diagnostics

let run_checks json =
  try
    let run = Process.run in
    let os = Platform.detect ~run () in
    let diagnostics =
      [ Platform.diagnostic os ]
      @ Check.command_diagnostics ~run
      @ Opam.diagnostics ~run os
      @ Editor.diagnostics ~run
    in
    print_string (render_diagnostics ~json diagnostics);
    Report.exit_code diagnostics
  with exn ->
    prerr_endline ("doctor internal failure: " ^ Printexc.to_string exn);
    3

let print_version () =
  print_endline Version.display;
  0

open Cmdliner

let json_output =
  let doc = "Print diagnostics as JSON." in
  Arg.(value & flag & info [ "json" ] ~doc)

let exit_infos =
  [
    Cmd.Exit.info ~doc:"no warnings or errors." 0;
    Cmd.Exit.info ~doc:"warnings only." 1;
    Cmd.Exit.info ~doc:"one or more errors." 2;
    Cmd.Exit.info ~doc:"unexpected internal failure." 3;
  ]
  @ List.filter
      (fun info -> Cmd.Exit.info_code info <> 0)
      Cmd.Exit.defaults

let check_cmd =
  let doc = "Run OCaml development environment diagnostics." in
  Cmd.v
    (Cmd.info "check" ~doc ~exits:exit_infos)
    Term.(const run_checks $ json_output)

let version_cmd =
  let doc = "Print the doctor version." in
  Cmd.v (Cmd.info "version" ~doc) Term.(const print_version $ const ())

let default_cmd ~name =
  let doc =
    "Inspect an OCaml development environment and print actionable \
     diagnostics."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "doctor checks for common OCaml, opam, dune, LSP, formatter, \
         shell environment, and VS Code setup issues. It does not \
         modify your machine.";
    ]
  in
  Cmd.group
    (Cmd.info name ~version:Version.current ~doc ~man ~exits:exit_infos)
    [ check_cmd; version_cmd ]

let main ~name () =
  match Cmd.eval_value' (default_cmd ~name) with
  | `Ok code -> code
  | `Exit code -> code
