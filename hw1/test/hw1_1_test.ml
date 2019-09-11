let rec sigma (a, b, f) =
  if a < b then f a + sigma (a + 1, b, f) else
  if a = b then f a else 0

(* test *)
let a = 10
let b = 10
let f x = x
let () = print_endline (string_of_int (sigma (a, b, f)))