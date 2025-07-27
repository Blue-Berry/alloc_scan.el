(* Test OCaml file to demonstrate alloc-scan plugin *)

let test_function x y =
  let result = x + y in
  Printf.printf "Result: %d\n" result

let main () =
  test_function 10 20;
  print_endline "Done"