type os = Windows | Macos | Linux | Wsl | Cygwin | Other of string

let env name = try Some (Sys.getenv name) with Not_found -> None

let contains_substring haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop index =
    if needle_length = 0 then true
    else if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  loop 0

let file_contains path needle =
  try
    let channel = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let needle = String.lowercase_ascii needle in
        let rec loop () =
          match input_line channel with
          | line ->
              contains_substring (String.lowercase_ascii line) needle
              || loop ()
          | exception End_of_file -> false
        in
        loop ())
  with Sys_error _ -> false

let has_wsl_marker () =
  env "WSL_DISTRO_NAME" <> None
  || file_contains "/proc/version" "microsoft"
  || file_contains "/proc/version" "wsl"

let detect ~(run : Process.runner) () =
  match Sys.os_type with
  | "Win32" -> Windows
  | "Cygwin" -> Cygwin
  | _ -> (
      let uname = run "uname" [ "-s" ] in
      match uname.status with
      | Process.Exited 0 -> (
          match String.lowercase_ascii (String.trim uname.stdout) with
          | "darwin" -> Macos
          | "linux" when has_wsl_marker () -> Wsl
          | "linux" -> Linux
          | other when other <> "" -> Other other
          | _ -> Other Sys.os_type)
      | _ -> Other Sys.os_type)

let to_string = function
  | Windows -> "Windows"
  | Macos -> "macOS"
  | Linux -> "Linux"
  | Wsl -> "WSL"
  | Cygwin -> "Cygwin"
  | Other value -> value

let diagnostic os =
  Check.make ~id:"platform.os"
    ~title:(Printf.sprintf "platform detected: %s" (to_string os))
    Check.Ok

let unix_like_shell = function
  | Macos | Linux | Wsl | Cygwin -> true
  | Windows | Other _ -> false

let environment_sync_suggestion os =
  if unix_like_shell os then "eval $(opam env)"
  else
    "Run `opam env` and apply the environment changes in your current \
     shell, then restart the terminal if needed."

let normalize_path path =
  let path =
    String.trim path |> String.map (function '\\' -> '/' | c -> c)
  in
  if Sys.win32 then String.lowercase_ascii path else path

let is_path_under ~parent path =
  let parent = normalize_path parent in
  let path = normalize_path path in
  let parent =
    if parent <> "" && parent.[String.length parent - 1] = '/' then
      parent
    else parent ^ "/"
  in
  String.length path >= String.length parent
  && String.sub path 0 (String.length parent) = parent

let command_locator os =
  match os with
  | Windows -> ("where", fun command -> [ command ])
  | _ -> ("sh", fun command -> [ "-c"; "command -v " ^ command ])
