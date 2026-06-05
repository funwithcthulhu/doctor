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

let diagnostics ?(env = Platform.env) os =
  path_current_directory_diagnostics ~env os
