let path_separator = function
  | Platform.Windows -> ';'
  | Macos | Linux | Wsl | Cygwin | Other _ -> ':'

let split_path os path =
  path
  |> String.split_on_char (path_separator os)
  |> List.map String.trim

let current_directory_path_entries os path =
  let entry_is_current_directory = function
    | "." -> true
    | "" -> Platform.unix_like_shell os
    | _ -> false
  in
  path |> split_path os |> List.filter entry_is_current_directory

let path_current_directory_diagnostics ?(env = Platform.env) os =
  match env "PATH" with
  | None -> []
  | Some path -> (
      match current_directory_path_entries os path with
      | [] -> []
      | entries ->
          [
            Check.make ~id:"env.path.current-directory"
              ~title:"PATH includes the current directory"
              ~detail:
                (Printf.sprintf "Current-directory PATH entries: %s"
                   (String.concat ", " entries))
              ~suggestion:
                "Remove `.` and empty current-directory entries from \
                 PATH before running opam builds."
              Check.Warn;
          ])

let normalized_value value =
  value |> String.trim |> String.lowercase_ascii

let enabled_env_value value =
  match normalized_value value with
  | "" | "0" | "false" | "no" -> false
  | _ -> true

let forced_clicolor_value value =
  match normalized_value value with
  | "always" | "force" | "forced" -> true
  | _ -> false

let variable_setting name value = Printf.sprintf "%s=%s" name value

let forced_color_variables env =
  let force_variables =
    [ "CLICOLOR_FORCE"; "CLICOLOR_FORCED" ]
    |> List.filter_map (fun name ->
        match env name with
        | Some value when enabled_env_value value ->
            Some (variable_setting name value)
        | _ -> None)
  in
  let clicolor =
    match env "CLICOLOR" with
    | Some value when forced_clicolor_value value ->
        [ variable_setting "CLICOLOR" value ]
    | _ -> []
  in
  force_variables @ clicolor

let forced_color_diagnostics ?(env = Platform.env) () =
  match forced_color_variables env with
  | [] -> []
  | variables ->
      [
        Check.make ~id:"env.color.forced"
          ~title:"forced color output is enabled"
          ~detail:
            (Printf.sprintf "Forced color variables: %s"
               (String.concat ", " variables))
          ~suggestion:
            "Unset forced color variables before running opam builds."
          Check.Warn;
      ]

let grep_options_diagnostics ?(env = Platform.env) () =
  match env "GREP_OPTIONS" with
  | Some value when String.trim value <> "" ->
      [
        Check.make ~id:"env.grep-options" ~title:"GREP_OPTIONS is set"
          ~detail:(variable_setting "GREP_OPTIONS" value)
          ~suggestion:"Unset GREP_OPTIONS before running opam builds."
          Check.Warn;
      ]
  | _ -> []

let diagnostics ?(env = Platform.env) os =
  path_current_directory_diagnostics ~env os
  @ forced_color_diagnostics ~env ()
  @ grep_options_diagnostics ~env ()
