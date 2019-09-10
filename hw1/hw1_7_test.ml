type expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval e =
  match e with
  | NUM i -> i
  | PLUS (e1, e2) -> eval e1 + eval e2
  | MINUS (e1, e2) -> eval e1 - eval e2
  | MULT (e1, e2) -> eval e1 * eval e2
  | DIVIDE (e1, e2) -> eval e1 / eval e2
  | MAX i ->
    match i with
    | [] -> 0
    | x :: y -> if (eval x) > (eval (MAX y)) then (eval x) else (eval (MAX y))

(* test *)
let test_e = MINUS(PLUS(PLUS(NUM 1, NUM 5), MULT(NUM 2, NUM 7)), DIVIDE(MINUS(NUM 20, NUM 2), MAX [MULT(NUM 2, NUM 2); NUM 1; DIVIDE(NUM 10, NUM 2); PLUS(NUM 1, NUM 2); MINUS(NUM 8, NUM 2)]))
let () = print_endline (string_of_int (eval test_e)) (* expected value is 17 *)