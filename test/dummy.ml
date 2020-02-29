let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (String.lowercase_ascii "hELLO!")

let () =
  let open Alcotest in
  run "Testing Module"
    [
      ("string-case",
       [ Alcotest.test_case "Lower_case" `Quick test_lowercase;
      ]);
    ]
