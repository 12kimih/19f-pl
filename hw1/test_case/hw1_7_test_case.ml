(* test *)
let test_expr = MINUS(PLUS(PLUS(NUM 1, NUM 5), MULT(NUM 2, NUM 7)), DIVIDE(MINUS(NUM 20, NUM 2), MAX [MULT(NUM 2, NUM 2); NUM 1; DIVIDE(NUM 10, NUM 2); PLUS(NUM 1, NUM 2); MINUS(NUM 8, NUM 2)]))
let () = print_endline (string_of_int (eval test_expr)) (* expected value is 17 *)