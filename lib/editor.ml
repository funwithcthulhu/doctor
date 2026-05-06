let has_extension extensions extension =
  extensions |> String.split_on_char '\n' |> List.map String.trim
  |> List.exists (String.equal extension)

let diagnostics ~(run : Process.runner) =
  let code = run "code" [ "--version" ] in
  match code.status with
  | Process.Spawn_error _ ->
      [
        Check.make ~id:"editor.vscode.command"
          ~title:"VS Code command not found (skipped)" Check.Ok;
      ]
  | Process.Exited 0 -> (
      let extensions = run "code" [ "--list-extensions" ] in
      match extensions.status with
      | Process.Exited 0
        when has_extension extensions.stdout "ocamllabs.ocaml-platform" ->
          [
            Check.make ~id:"editor.vscode.ocaml-platform"
              ~title:"VS Code OCaml Platform extension detected" Check.Ok;
          ]
      | Process.Exited 0 ->
          [
            Check.make ~id:"editor.vscode.ocaml-platform"
              ~title:"VS Code OCaml Platform extension not detected"
              ~suggestion:
                "Install extension ocamllabs.ocaml-platform in VS Code."
              Check.Warn;
          ]
      | _ ->
          [
            Check.make ~id:"editor.vscode.extensions"
              ~title:"could not list VS Code extensions"
              ~detail:(Process.summary extensions)
              ~suggestion:
                "Open VS Code and check whether ocamllabs.ocaml-platform is \
                 installed."
              Check.Warn;
          ])
  | _ ->
      [
        Check.make ~id:"editor.vscode.command"
          ~title:"VS Code command exists but could not run"
          ~detail:(Process.summary code)
          ~suggestion:
            "Try running `code --version`, or reinstall the VS Code \
             command-line launcher."
          Check.Warn;
      ]
