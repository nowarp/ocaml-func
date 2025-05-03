open Alcotest

let contracts_dir = "../../../test/contracts"
let contract_path name =
  Filename.concat contracts_dir name

let test_parse_success file () =
  match Func.parse (contract_path file) with
  | Ok _ -> ()
  | Error e ->
      failf "Failed to parse %s: %s" file e

let test_parser = [
  test_case "Parse global variables" `Quick (test_parse_success "func_frontend.fc");
]

let () =
  run "Parser Tests" [
    "parser", test_parser;
  ]
