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
      | hd :: tl -> SUM [TIMES [diff (hd, var); TIMES tl]; TIMES[hd; diff (TIMES tl, var)]] )
  | SUM ae_list ->
    ( match ae_list with
      | [] -> raise InvalidArgument
      | hd :: tl -> SUM [diff (hd, var); diff (SUM tl, var)] )