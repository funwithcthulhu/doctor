let non_empty_lines output =
  output
  |> String.split_on_char '\n'
  |> List.map String.trim
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
      if String.starts_with ~prefix:"[error]" lower then None
      else Some line

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

let has_package packages package =
  List.exists (String.equal package) packages

type package_state =
  | Installed_packages of string list
  | Package_query_failed of Process.result

let installed_packages = function
  | Installed_packages packages -> Some packages
  | Package_query_failed _ -> None

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
          Check.make ~id:"opam.initialized"
            ~title:"opam root could not be read"
            ~detail:(Process.summary result)
            ~suggestion:"opam init" Check.Warn)
  | _ ->
      Check.make ~id:"opam.initialized"
        ~title:"opam does not appear initialized"
        ~detail:(Process.summary result)
        ~suggestion:"opam init" Check.Warn

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
            Check.make ~id:"opam.switch.active"
              ~title:"opam switch not active"
              ~detail:"opam did not report an active switch."
              ~suggestion Check.Error)
    | _ ->
        let suggestion = switch_suggestion os switch_list in
        Check.make ~id:"opam.switch.active"
          ~title:"opam switch not active" ~detail:(Process.summary show)
          ~suggestion Check.Error
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
        Check.make ~id:"opam.switch.list"
          ~title:"could not list opam switches"
          ~detail:(Process.summary switches)
          ~suggestion:"Run `opam switch list` to inspect your switches."
          Check.Warn
  in
  [ show_diagnostic; list_diagnostic ]

let locate_command ~(run : Process.runner) os command =
  let locator, args_for = Platform.command_locator os in
  first_stdout_line (run locator (args_for command))

let locate_ocaml ~run os = locate_command ~run os "ocaml"

type switch_tool = {
  label : string;
  commands : string list;
  package : string option;
}

let switch_tools =
  [
    { label = "ocaml"; commands = [ "ocaml" ]; package = None };
    { label = "dune"; commands = [ "dune" ]; package = Some "dune" };
    {
      label = "OCaml LSP";
      commands = [ "ocaml-lsp-server"; "ocamllsp" ];
      package = Some "ocaml-lsp-server";
    };
    {
      label = "ocamlformat";
      commands = [ "ocamlformat" ];
      package = Some "ocamlformat";
    };
  ]

let tool_expected packages tool =
  match (tool.package, packages) with
  | None, _ -> true
  | Some package, Some packages -> has_package packages package
  | Some _, None -> false

let visible_tool_path ~(run : Process.runner) os tool =
  List.find_map (locate_command ~run os) tool.commands

let env_detail ~switch_bin ~missing ~outside =
  let missing_lines =
    match missing with
    | [] -> []
    | _ :: _ ->
        [
          Printf.sprintf "Commands missing from PATH: %s."
            (String.concat ", " missing);
        ]
  in
  let outside_lines =
    match outside with
    | [] -> []
    | _ :: _ ->
        "Commands resolving outside the active switch:"
        :: List.map
             (fun (label, path) -> Printf.sprintf "%s: %s" label path)
             outside
  in
  String.concat "\n"
    ((("Active switch bin: " ^ switch_bin) :: missing_lines)
    @ outside_lines)

let env_sync_diagnostic ~(run : Process.runner) os package_state =
  let active_switch =
    match run "opam" [ "switch"; "show" ] with
    | { status = Process.Exited 0; stdout; _ } ->
        parse_active_switch stdout
    | _ -> None
  in
  match active_switch with
  | None -> []
  | Some _ -> (
      let bin = run "opam" [ "var"; "bin" ] in
      match first_stdout_line bin with
      | None ->
          [
            Check.make ~id:"opam.env.sync"
              ~title:"active switch bin could not be read"
              ~detail:(Process.summary bin)
              ~suggestion:
                "Run `opam var bin` to inspect the active switch."
              Check.Warn;
          ]
      | Some switch_bin -> (
          let expected_tools =
            switch_tools
            |> List.filter
                 (tool_expected (installed_packages package_state))
          in
          let missing, outside =
            List.fold_left
              (fun (missing, outside) tool ->
                match visible_tool_path ~run os tool with
                | None -> (tool.label :: missing, outside)
                | Some path
                  when Platform.is_path_under ~parent:switch_bin path ->
                    (missing, outside)
                | Some path -> (missing, (tool.label, path) :: outside))
              ([], []) expected_tools
          in
          match (List.rev missing, List.rev outside) with
          | [], [] ->
              let detail =
                match locate_ocaml ~run os with
                | Some path ->
                    Printf.sprintf "ocaml resolves to %s" path
                | None ->
                    Printf.sprintf "Active switch bin: %s" switch_bin
              in
              [
                Check.make ~id:"opam.env.sync"
                  ~title:
                    "checked OCaml tools resolve through the active \
                     opam switch"
                  ~detail Check.Ok;
              ]
          | missing, outside ->
              [
                Check.make ~id:"opam.env.sync"
                  ~title:
                    "shell environment may not include the active opam \
                     switch"
                  ~detail:(env_detail ~switch_bin ~missing ~outside)
                  ~suggestion:(Platform.environment_sync_suggestion os)
                  Check.Warn;
              ]))

let package_diagnostic packages package ~optional =
  if has_package packages package then
    Check.make
      ~id:("opam.package." ^ package)
      ~title:(Printf.sprintf "%s package installed" package)
      Check.Ok
  else
    let title =
      if optional then
        Printf.sprintf "%s not installed (optional)" package
      else Printf.sprintf "%s not installed" package
    in
    Check.make
      ~id:("opam.package." ^ package)
      ~title
      ~suggestion:(Printf.sprintf "opam install %s" package)
      Check.Warn

let read_package_state ~(run : Process.runner) =
  let result = run "opam" [ "list"; "--installed"; "--short" ] in
  match result.status with
  | Process.Exited 0 ->
      Installed_packages (parse_installed_packages result.stdout)
  | _ -> Package_query_failed result

let package_diagnostics = function
  | Installed_packages packages ->
      [
        package_diagnostic packages "dune" ~optional:false;
        package_diagnostic packages "ocaml-lsp-server" ~optional:false;
        package_diagnostic packages "ocamlformat" ~optional:false;
        package_diagnostic packages "utop" ~optional:true;
      ]
  | Package_query_failed result ->
      [
        Check.make ~id:"opam.packages"
          ~title:"could not read installed opam packages"
          ~detail:(Process.summary result)
          ~suggestion:
            "Run `opam list --installed --short` to inspect packages."
          Check.Warn;
      ]

let windows_path parent child =
  let separator =
    if String.ends_with ~suffix:"\\" parent then "" else "\\"
  in
  parent ^ separator ^ child

let powershell_quote value =
  "'" ^ String.concat "''" (String.split_on_char '\'' value) ^ "'"

let powershell_array values =
  "@(" ^ String.concat ", " (List.map powershell_quote values) ^ ")"

let doctor_plugin_probe_script ~root ~switch_bin =
  let plugin =
    windows_path
      (windows_path (windows_path root "plugins") "bin")
      "opam-doctor.exe"
  in
  let target = windows_path switch_bin "opam-doctor.exe" in
  String.concat "; "
    [
      "$plugin = " ^ powershell_quote plugin;
      "$target = " ^ powershell_quote target;
    ]
  ^ "; "
  ^ String.concat " "
      [
        "if (!(Test-Path -LiteralPath $plugin) -and !(Test-Path \
         -LiteralPath $target)) { 'absent' }";
        "elseif (!(Test-Path -LiteralPath $plugin)) { 'plugin-missing' \
         }";
        "elseif (!(Test-Path -LiteralPath $target)) { 'target-missing' \
         }";
        "else { $item = Get-Item -LiteralPath $plugin; if \
         ($item.LinkType -ne 'SymbolicLink') { 'not-symlink' } else { \
         $rawTarget = [string]$item.Target; if \
         ([System.IO.Path]::IsPathRooted($rawTarget)) { $resolved = \
         [System.IO.Path]::GetFullPath($rawTarget) } else { $resolved \
         = [System.IO.Path]::GetFullPath((Join-Path (Split-Path \
         -Parent $plugin) $rawTarget)) }; $expected = \
         [System.IO.Path]::GetFullPath($target); if \
         ([string]::Equals($resolved, $expected, \
         [System.StringComparison]::OrdinalIgnoreCase)) { 'ok' } else \
         { 'wrong-target'; 'Target: ' + $rawTarget; 'Expected: ' + \
         $target } } }";
      ]

let doctor_plugin_probe ~(run : Process.runner) ~root ~switch_bin =
  run "powershell"
    [
      "-NoProfile";
      "-NonInteractive";
      "-ExecutionPolicy";
      "Bypass";
      "-Command";
      doctor_plugin_probe_script ~root ~switch_bin;
    ]

let windows_symlink_probe_script =
  String.concat "; "
    [
      "$path = \
       'HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\AppModelUnlock'";
      "$value = (Get-ItemProperty -Path $path -Name \
       AllowDevelopmentWithoutDevLicense -ErrorAction \
       SilentlyContinue).AllowDevelopmentWithoutDevLicense";
      "if ($value -eq 1) { 'enabled' } else { 'disabled' }";
    ]

let windows_symlink_probe ~(run : Process.runner) =
  run "powershell"
    [
      "-NoProfile";
      "-NonInteractive";
      "-ExecutionPolicy";
      "Bypass";
      "-Command";
      windows_symlink_probe_script;
    ]

let windows_symlink_diagnostic_from_probe result =
  match first_stdout_line result with
  | Some "enabled" ->
      [
        Check.make ~id:"opam.windows.symlink"
          ~title:"Windows user symlink support is enabled"
          ~detail:
            "Developer Mode symlink support is enabled for this \
             Windows installation."
          Check.Ok;
      ]
  | Some "disabled" ->
      [
        Check.make ~id:"opam.windows.symlink"
          ~title:"Windows user symlink support may be disabled"
          ~detail:
            "The Windows Developer Mode symlink setting is not \
             enabled. opam plugin entries may be copied instead of \
             linked."
          ~suggestion:
            "Enable Windows Developer Mode or run `opam reinstall \
             doctor` from an elevated shell."
          Check.Warn;
      ]
  | _ ->
      [
        Check.make ~id:"opam.windows.symlink"
          ~title:"could not inspect Windows symlink support"
          ~detail:(Process.summary result)
          ~suggestion:
            "If opam plugin dispatch fails, check whether Windows \
             Developer Mode allows user symlink creation."
          Check.Warn;
      ]

let windows_symlink_diagnostics ~(run : Process.runner) os =
  match os with
  | Platform.Windows ->
      windows_symlink_probe ~run
      |> windows_symlink_diagnostic_from_probe
  | _ -> []

let windows_plugin_runtime_dirs root =
  let usr =
    windows_path
      (windows_path (windows_path root ".cygwin") "root")
      "usr"
  in
  let runtime_dir triplet =
    let sys_root = windows_path (windows_path usr triplet) "sys-root" in
    windows_path (windows_path sys_root "mingw") "bin"
  in
  List.map runtime_dir [ "x86_64-w64-mingw32"; "i686-w64-mingw32" ]

let windows_runtime_path_probe_script ~root =
  let dirs = windows_plugin_runtime_dirs root in
  String.concat "; "
    [
      "$dirs = " ^ powershell_array dirs;
      "function Normalize($path) { \
       [System.IO.Path]::GetFullPath($path).TrimEnd('\\', '/') }";
      "$pathEntries = @($env:Path -split ';' | Where-Object { $_ -ne \
       '' } | ForEach-Object { Normalize $_ })";
      "$existing = @()";
      "$missing = @()";
      "foreach ($dir in $dirs) { if (Test-Path -LiteralPath $dir \
       -PathType Container) { $existing += $dir; $full = Normalize \
       $dir; $found = $false; foreach ($entry in $pathEntries) { if \
       ([string]::Equals($entry, $full, \
       [System.StringComparison]::OrdinalIgnoreCase)) { $found = $true \
       } }; if (!$found) { $missing += $dir } } }";
    ]
  ^ "; "
  ^ String.concat " "
      [
        "if ($existing.Count -eq 0) { 'absent' }";
        "elseif ($missing.Count -eq 0) { 'ok' }";
        "else { 'missing'; $missing }";
      ]

let windows_runtime_path_probe ~(run : Process.runner) ~root =
  run "powershell"
    [
      "-NoProfile";
      "-NonInteractive";
      "-ExecutionPolicy";
      "Bypass";
      "-Command";
      windows_runtime_path_probe_script ~root;
    ]

let windows_runtime_path_diagnostic_from_probe result =
  match first_stdout_line result with
  | Some "absent" -> []
  | Some "ok" ->
      [
        Check.make ~id:"opam.windows.runtime-path"
          ~title:"opam Windows runtime directories are on PATH"
          ~detail:
            "The expected opam Windows runtime directories are present \
             in the current PATH."
          Check.Ok;
      ]
  | Some "missing" ->
      let missing =
        match non_empty_lines result.stdout with
        | _state :: paths -> paths
        | [] -> []
      in
      [
        Check.make ~id:"opam.windows.runtime-path"
          ~title:
            "opam plugin runtime directories may be missing from PATH"
          ~detail:
            (String.concat "\n"
               ("Runtime directories missing from PATH:" :: missing))
          ~suggestion:
            "Add the missing opam runtime directories to your Windows \
             user PATH, then open a new shell."
          Check.Warn;
      ]
  | _ ->
      [
        Check.make ~id:"opam.windows.runtime-path"
          ~title:"could not inspect opam plugin runtime PATH"
          ~detail:(Process.summary result)
          ~suggestion:
            "If `opam doctor` fails to start on Windows, check whether \
             opam runtime directories are present in PATH."
          Check.Warn;
      ]

let windows_runtime_path_diagnostics ~(run : Process.runner) ~root os =
  match os with
  | Platform.Windows ->
      windows_runtime_path_probe ~run ~root
      |> windows_runtime_path_diagnostic_from_probe
  | _ -> []

let doctor_plugin_paths ~root ~switch_bin =
  let plugin =
    windows_path
      (windows_path (windows_path root "plugins") "bin")
      "opam-doctor.exe"
  in
  let target = windows_path switch_bin "opam-doctor.exe" in
  (plugin, target)

let doctor_plugin_detail ~plugin ~target extra =
  String.concat "\n"
    ([ "Plugin entry: " ^ plugin; "Switch binary: " ^ target ] @ extra)

let doctor_plugin_reinstall_suggestion =
  "Enable Windows Developer Mode or use an elevated shell if symlink \
   creation is unavailable, then run `opam reinstall doctor`."

let doctor_plugin_diagnostic_from_probe ~root ~switch_bin result =
  let plugin, target = doctor_plugin_paths ~root ~switch_bin in
  match first_stdout_line result with
  | Some "absent" -> []
  | Some "ok" ->
      [
        Check.make ~id:"opam.plugin.doctor"
          ~title:"opam doctor plugin dispatch looks usable"
          ~detail:(doctor_plugin_detail ~plugin ~target [])
          Check.Ok;
      ]
  | Some "plugin-missing" ->
      [
        Check.make ~id:"opam.plugin.doctor"
          ~title:"opam doctor plugin entry missing"
          ~detail:(doctor_plugin_detail ~plugin ~target [])
          ~suggestion:"opam reinstall doctor" Check.Warn;
      ]
  | Some "target-missing" ->
      [
        Check.make ~id:"opam.plugin.doctor"
          ~title:"opam doctor plugin target missing"
          ~detail:(doctor_plugin_detail ~plugin ~target [])
          ~suggestion:"opam reinstall doctor" Check.Warn;
      ]
  | Some "not-symlink" ->
      [
        Check.make ~id:"opam.plugin.doctor"
          ~title:"opam doctor plugin entry is not a symlink"
          ~detail:(doctor_plugin_detail ~plugin ~target [])
          ~suggestion:doctor_plugin_reinstall_suggestion Check.Warn;
      ]
  | Some "wrong-target" ->
      let extra =
        match non_empty_lines result.stdout with
        | _state :: rest -> rest
        | [] -> []
      in
      [
        Check.make ~id:"opam.plugin.doctor"
          ~title:"opam doctor plugin entry points at a different target"
          ~detail:(doctor_plugin_detail ~plugin ~target extra)
          ~suggestion:doctor_plugin_reinstall_suggestion Check.Warn;
      ]
  | _ ->
      [
        Check.make ~id:"opam.plugin.doctor"
          ~title:"could not inspect opam doctor plugin dispatch"
          ~detail:(Process.summary result)
          ~suggestion:
            "Run `opam doctor version` to test plugin dispatch."
          Check.Warn;
      ]

let doctor_plugin_diagnostics ~(run : Process.runner) os =
  match os with
  | Platform.Windows -> (
      match
        ( first_stdout_line (run "opam" [ "var"; "root" ]),
          first_stdout_line (run "opam" [ "var"; "bin" ]) )
      with
      | Some root, Some switch_bin ->
          let probe = doctor_plugin_probe ~run ~root ~switch_bin in
          let plugin =
            doctor_plugin_diagnostic_from_probe ~root ~switch_bin probe
          in
          let symlink =
            match first_stdout_line probe with
            | Some "absent" -> []
            | _ -> windows_symlink_diagnostics ~run os
          in
          let runtime_path =
            match first_stdout_line probe with
            | Some "absent" -> []
            | _ -> windows_runtime_path_diagnostics ~run ~root os
          in
          plugin @ symlink @ runtime_path
      | _ -> [])
  | _ -> []

let diagnostics ~(run : Process.runner) os =
  if opam_available ~run then
    let package_state = read_package_state ~run in
    [ initialized_diagnostic ~run ]
    @ switch_diagnostics ~run os
    @ env_sync_diagnostic ~run os package_state
    @ doctor_plugin_diagnostics ~run os
    @ package_diagnostics package_state
  else
    [
      Check.make ~id:"opam.initialized"
        ~title:"opam checks skipped because opam is missing"
        ~suggestion:
          "Install opam from https://opam.ocaml.org/doc/Install.html"
        Check.Error;
    ]
