module A = Alcotest

let test_case = ()

let () =
  A.run
    "What package"
    [ "testsuite1", [ A.test_case "test1" `Quick test_case ] ]
;;
