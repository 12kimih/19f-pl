type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec calculate e =
  let rec substitute e n =
    match e with
    | X -> n
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD (e1, e2) -> substitute e1 n +. substitute e2 n
    | SUB (e1, e2) -> substitute e1 n -. substitute e2 n
    | MUL (e1, e2) -> substitute e1 n *. substitute e2 n
    | DIV (e1, e2) -> substitute e1 n /. substitute e2 n
    | SIGMA (e1, e2, e3) ->
      let e1_val = int_of_float (substitute e1 n) in
      let e2_val = int_of_float (substitute e2 n) in
      if e1_val <= e2_val then substitute e3 (float_of_int e1_val) +. substitute (SIGMA (INT (e1_val + 1), INT e2_val, e3)) n else 0.0
    | INTEGRAL (e1, e2, e3) ->
      let e1_val = substitute e1 n in
      let e2_val = substitute e2 n in
      if e1_val > e2_val +. 0.1 then ~-.(substitute (INTEGRAL (REAL e2_val, REAL e1_val, e3)) n) else
      if e1_val < e2_val -. 0.1 then substitute e3 e1_val *. 0.1 +. substitute (INTEGRAL (REAL (e1_val +. 0.1), REAL e2_val, e3)) n else 0.0
  in
  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> calculate(e1) +. calculate(e2)
  | SUB (e1, e2) -> calculate(e1) -. calculate(e2)
  | MUL (e1, e2) -> calculate(e1) *. calculate(e2)
  | DIV (e1, e2) -> calculate(e1) /. calculate(e2)
  | SIGMA (e1, e2, e3) ->
    let e1_val = int_of_float (calculate e1) in
    let e2_val = int_of_float (calculate e2) in
    if e1_val <= e2_val then substitute e3 (float_of_int e1_val) +. calculate (SIGMA (INT (e1_val + 1), INT e2_val, e3)) else 0.0
  | INTEGRAL (e1, e2, e3) ->
    let e1_val = calculate e1 in
    let e2_val = calculate e2 in
    if e1_val > e2_val +. 0.1 then ~-.(calculate (INTEGRAL (REAL e2_val, REAL e1_val, e3))) else
    if e1_val < e2_val -. 0.1 then substitute e3 e1_val *. 0.1 +. calculate (INTEGRAL (REAL (e1_val +. 0.1), REAL e2_val, e3)) else 0.0