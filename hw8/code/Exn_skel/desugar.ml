(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | If (e1, e2, e3) -> 
    If (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Equal (e1, e2) -> Equal (alpha_conv e1 subst, alpha_conv e2 subst)
  | Raise e -> Raise (alpha_conv e subst)
  | Handle (e1, n, e2) -> Handle (alpha_conv e1 subst, n, alpha_conv e2 subst)

let rec cps : xexp -> xexp = fun e ->
  let k = new_name () in
  let h = new_name () in
  match e with
  (* Constant expressions *)
  | Num n -> Fn (k, Fn (h, App (Var k, Num n)))
  | Var x -> Fn (k, Fn (h, App (Var k, Var x)))
  | Fn (x, e) -> Fn (k, Fn (h, App (Var k, Fn (x, cps e))))
  (* Non constant expressions *)
  | App (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v1, App (App (cps e2, Fn (v2, App (App (App (Var v1, Var v2), Var k), Var h))), Var h))), Var h)))
  | If (e1, e2, e3) ->
    let v = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v, If (Var v, App (App (cps e2, Var k), Var h), App (App (cps e3, Var k), Var h)))), Var h)))
  | Equal (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v1, App (App (cps e2, Fn (v2, App (Var k, Equal (Var v1, Var v2)))), Var h))), Var h)))
  | Raise e ->
    Fn (k, Fn (h, App (App (cps e, Var h), Var h)))
  | Handle (e1, n, e2) ->
    let v = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Var k), Fn (v, If (Equal (Var v, Num n), App (App (cps e2, Var k), Var h), App (Var h, Var v))))))

let removeExn : xexp -> xexp = fun e ->
  let v1 = new_name () in
  let v2 = new_name () in
  App (App (cps (alpha_conv e []), Fn (v1, Var v1)), Fn (v2, Num 201912))
