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