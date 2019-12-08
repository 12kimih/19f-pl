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
  | Fn (x, e) ->
    let v = new_name () in
    Fn (k, Fn (h, App (Var k, Fn (x, App (App (cps e, Fn (v, Var v)), Var h)))))
  (* Non constant expressions *)
  | App (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v1, App (App (cps e2, Fn (v2, App (Var k, App (Var v1, Var v2)))), Var h))), Var h)))
  | If (e1, e2, e3) ->
    let v1 = new_name () in
    let v2 = new_name () in
    let v3 = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v1, If (Var v1, App (App (cps e2, Fn (v2, App (Var k, Var v2))), Var h), App (App (cps e3, Fn (v3, App (Var k, Var v3))), Var h)))), Var h)))
  | Equal (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v1, App (App (cps e2, Fn (v2, App (Var k, Equal (Var v1, Var v2)))), Var h))), Var h)))
  | Raise e ->
    let v = new_name () in
    Fn (k, Fn (h, App (App (cps e, Fn (v, App (Var h, Var v))), Var h)))
  | Handle (e1, n, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    let v3 = new_name () in
    Fn (k, Fn (h, App (App (cps e1, Fn (v1, App (Var k, Var v1))), Fn (v2, If (Equal (Var v2, Num n), App (App (cps e2, Fn (v3, App (Var k, Var v3))), Var h), App (Var h, Var v2))))))

let removeExn : xexp -> xexp = fun e ->
  let v1 = new_name () in
  let v2 = new_name () in
  App (App (cps (alpha_conv e []), Fn (v1, Var v1)), Fn (v2, Num 201912))
