let expect_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let () =
  expect_equal "version display" "ocaml-doctor 0.1.0"
    Ocaml_doctor.Version.display
