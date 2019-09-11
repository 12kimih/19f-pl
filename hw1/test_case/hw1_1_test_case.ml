(* test *)
let a = 11
let b = 10
let f x = x
let () = print_endline (string_of_int (sigma (a, b, f)))