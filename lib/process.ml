type status =
  | Exited of int
  | Signaled of int
  | Stopped of int
  | Spawn_error of string

type result = {
  command : string;
  args : string list;
  status : status;
  stdout : string;
  stderr : string;
}

type runner = string -> string list -> result

let status_to_string = function
  | Exited code -> Printf.sprintf "exit %d" code
  | Signaled signal -> Printf.sprintf "signal %d" signal
  | Stopped signal -> Printf.sprintf "stopped by signal %d" signal
  | Spawn_error message -> Printf.sprintf "spawn error: %s" message

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let quote_for_display value =
  if value = "" || String.exists is_whitespace value then
    Printf.sprintf "%S" value
  else value

let command_line command args =
  String.concat " " (List.map quote_for_display (command :: args))

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let remove_if_exists path =
  try Sys.remove path with
  | Sys_error _ -> ()

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let null_device = if Sys.win32 then "NUL" else "/dev/null"

let unix_status_to_status = function
  | Unix.WEXITED code -> Exited code
  | WSIGNALED signal -> Signaled signal
  | WSTOPPED signal -> Stopped signal

let run command args =
  let stdout_path = Filename.temp_file "doctor-" ".stdout" in
  let stderr_path = Filename.temp_file "doctor-" ".stderr" in
  let stdin_fd = Unix.openfile null_device [ Unix.O_RDONLY ] 0 in
  let stdout_fd =
    Unix.openfile stdout_path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o600
  in
  let finish status =
    close_noerr stdin_fd;
    close_noerr stdout_fd;
    close_noerr stderr_fd;
    let stdout = read_file stdout_path in
    let stderr = read_file stderr_path in
    remove_if_exists stdout_path;
    remove_if_exists stderr_path;
    { command; args; status; stdout; stderr }
  in
  try
    let argv = Array.of_list (command :: args) in
    let env = Unix.environment () in
    let pid =
      Unix.create_process_env command argv env stdin_fd stdout_fd stderr_fd
    in
    close_noerr stdin_fd;
    close_noerr stdout_fd;
    close_noerr stderr_fd;
    let _pid, status = Unix.waitpid [] pid in
    let stdout = read_file stdout_path in
    let stderr = read_file stderr_path in
    remove_if_exists stdout_path;
    remove_if_exists stderr_path;
    { command; args; status = unix_status_to_status status; stdout; stderr }
  with
  | Unix.Unix_error (error, function_name, argument) ->
      let message =
        Printf.sprintf "%s: %s %s" (Unix.error_message error) function_name
          argument
      in
      finish (Spawn_error message)

let trim_for_summary text =
  text
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line -> line <> "")
  |> String.concat " "

let summary result =
  let status = status_to_string result.status in
  let stderr = trim_for_summary result.stderr in
  let stdout = trim_for_summary result.stdout in
  match (stdout, stderr) with
  | "", "" -> Printf.sprintf "%s returned %s" (command_line result.command result.args) status
  | "", stderr ->
      Printf.sprintf "%s returned %s: %s"
        (command_line result.command result.args)
        status stderr
  | stdout, "" ->
      Printf.sprintf "%s returned %s: %s"
        (command_line result.command result.args)
        status stdout
  | stdout, stderr ->
      Printf.sprintf "%s returned %s: %s / %s"
        (command_line result.command result.args)
        status stdout stderr
