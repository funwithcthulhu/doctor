let expect_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_bool label value =
  if not value then failwith (Printf.sprintf "%s: expected true" label)

let expect_false label value =
  if value then failwith (Printf.sprintf "%s: expected false" label)

let expect_some label = function
  | Some value -> value
  | None -> failwith (Printf.sprintf "%s: expected Some _" label)

let expect_none label = function
  | Some value ->
      failwith (Printf.sprintf "%s: expected None, got %S" label value)
  | None -> ()

let expect_locator label os expected_command expected_args =
  let command, args_for = Doctor.Platform.command_locator os in
  expect_equal (label ^ " command") expected_command command;
  expect_equal (label ^ " args")
    (String.concat "\000" expected_args)
    (String.concat "\000" (args_for "ocaml"))

let result ?(stdout = "") ?(stderr = "") status command args =
  { Doctor.Process.command; args; status; stdout; stderr }

let fake_runner responses command args =
  match List.assoc_opt (command, args) responses with
  | Some (status, stdout, stderr) ->
      result ~stdout ~stderr status command args
  | None -> result (Doctor.Process.Spawn_error "not found") command args

let () =
  expect_equal "command line quoting" "opam \"switch show\""
    (Doctor.Process.command_line "opam" [ "switch show" ]);
  expect_equal "ocaml version parsing" "5.2.0"
    (Doctor.Check.parse_ocaml_version
       "The OCaml toplevel, version 5.2.0");
  expect_equal "active switch parsing" "5.2.0"
    (expect_some "active switch parsing"
       (Doctor.Opam.parse_active_switch "5.2.0\n"));
  expect_bool "no active switch"
    (Doctor.Opam.parse_active_switch "[ERROR] No switch\n" = None);
  expect_bool "package parser finds dune"
    ( Doctor.Opam.parse_installed_packages
        "ocaml\nbase-unix\ndune\nocaml-lsp-server\n"
    |> fun packages -> Doctor.Opam.has_package packages "dune" );
  expect_equal "switch list parser" "default, 5.2.0"
    (Doctor.Opam.parse_switch_list "default\n5.2.0\n"
    |> String.concat ", ");
  expect_equal "active switch marker is trimmed" "default, 5.2.0"
    (Doctor.Opam.parse_switch_list "* default\n  5.2.0\n"
    |> String.concat ", ");
  expect_equal "package parser trims whitespace" "dune, ocamlformat"
    (Doctor.Opam.parse_installed_packages
       "  dune   3.17.0\n\tocamlformat\t0.27.0\n"
    |> String.concat ", ");
  expect_bool "path below switch bin"
    (Doctor.Platform.is_path_under ~parent:"/home/me/.opam/5.2.0/bin"
       "/home/me/.opam/5.2.0/bin/ocaml");
  expect_false "path with shared prefix is not below switch bin"
    (Doctor.Platform.is_path_under ~parent:"/home/me/.opam/5.2.0/bin"
       "/home/me/.opam/5.2.0/bin-old/ocaml");
  expect_bool "macOS switch path below bin"
    (Doctor.Platform.is_path_under ~parent:"/Users/me/.opam/5.2.0/bin"
       "/Users/me/.opam/5.2.0/bin/ocaml");
  expect_bool "Linux switch path below bin"
    (Doctor.Platform.is_path_under ~parent:"/home/me/.opam/default/bin"
       "/home/me/.opam/default/bin/dune");
  expect_bool "Windows switch path below bin"
    (Doctor.Platform.is_path_under ~parent:"C:\\opam\\default\\bin"
       "C:\\opam\\default\\bin\\ocaml.exe");
  expect_false "Windows different bin is outside switch"
    (Doctor.Platform.is_path_under ~parent:"C:\\opam\\default\\bin"
       "C:\\OCaml\\bin\\ocaml.exe");
  expect_locator "macOS command locator" Doctor.Platform.Macos "sh"
    [ "-c"; "command -v ocaml" ];
  expect_locator "Linux command locator" Doctor.Platform.Linux "sh"
    [ "-c"; "command -v ocaml" ];
  expect_locator "Windows command locator" Doctor.Platform.Windows
    "where" [ "ocaml" ];
  expect_equal "executable present" "/home/me/.opam/default/bin/ocaml"
    (expect_some "executable present"
       (Doctor.Opam.locate_command
          ~run:
            (fake_runner
               [
                 ( ("sh", [ "-c"; "command -v ocaml" ]),
                   ( Doctor.Process.Exited 0,
                     "/home/me/.opam/default/bin/ocaml\n",
                     "" ) );
               ])
          Doctor.Platform.Linux "ocaml"));
  expect_none "executable absent"
    (Doctor.Opam.locate_command ~run:(fake_runner [])
       Doctor.Platform.Linux "ocaml");
  expect_none "empty PATH-like result"
    (Doctor.Opam.locate_command
       ~run:
         (fake_runner
            [
              ( ("sh", [ "-c"; "command -v ocaml" ]),
                (Doctor.Process.Exited 1, "", "") );
            ])
       Doctor.Platform.Linux "ocaml");
  expect_none "whitespace-only locator output"
    (Doctor.Opam.locate_command
       ~run:
         (fake_runner
            [
              ( ("sh", [ "-c"; "command -v ocaml" ]),
                (Doctor.Process.Exited 0, " \n\t\n", "") );
            ])
       Doctor.Platform.Linux "ocaml");
  expect_equal "duplicate PATH entries still resolve first match"
    "/bin/ocaml"
    (expect_some "duplicate PATH entries"
       (Doctor.Opam.locate_command
          ~run:
            (fake_runner
               [
                 ( ("sh", [ "-c"; "command -v ocaml" ]),
                   ( Doctor.Process.Exited 0,
                     "/bin/ocaml\n/bin/ocaml\n",
                     "" ) );
               ])
          Doctor.Platform.Linux "ocaml"))
