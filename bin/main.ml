

let () =
  print_endline Lib.hello;
  print_endline "Chapter2 In Progress";
  print_endline @@ Bool.to_string @@ Chapter2.(List_as_stack.is_empty List_as_stack.empty)
