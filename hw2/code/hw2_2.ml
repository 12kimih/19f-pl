type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

exception InvalidArgument

let rec diff (e, var) =
  match e with
  | CONST i -> CONST 0
  | VAR e_var -> if e_var = var then CONST 1 else CONST 0
  | POWER (e_var, i) ->
    if e_var <> var then CONST 0 else
    if i = 0 then CONST 0 else TIMES [CONST i; POWER (e_var, i - 1)]
  | TIMES ae_list ->
    ( match ae_list with
      | [] -> raise InvalidArgument
      | hd :: tl -> 
      ( match tl with
        | [] -> diff (hd, var)
        | _ -> SUM [TIMES [diff (hd, var); TIMES tl]; TIMES[hd; diff (TIMES tl, var)]] ) )
  | SUM ae_list ->
    ( match ae_list with
      | [] -> raise InvalidArgument
      | hd :: tl -> 
      ( match tl with
        | [] -> diff (hd, var)
        | _ -> SUM [diff (hd, var); diff (SUM tl, var)] ) )


let _ = diff (VAR "X", "X")

let _ = diff (VAR "Y", "X")

let _ = diff (SUM [VAR "X"; CONST 3; POWER ("X", 3); POWER ("Y", 2)], "X")

let _ = diff (TIMES [VAR "X"; SUM [VAR "X"; CONST 2]; POWER ("X", 2)], "X")

let _ = diff (SUM [TIMES [VAR "a"; POWER ("X", 2)]; TIMES [VAR "b"; VAR "X"]; VAR "c"], "X")