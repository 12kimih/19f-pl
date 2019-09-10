let rec iter (n, f) x =
  if n > 0 then f (iter (n - 1, f) x) else x
let f x = x + 1
let () = print_endline (string_of_int (iter (10, f) 10))
