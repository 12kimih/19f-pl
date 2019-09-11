(* test *)
let f x = x + 1
let () = print_endline (string_of_int (iter (10, f) 10))