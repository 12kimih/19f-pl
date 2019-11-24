(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TEqual of var
  | TWrite of var

let assoc_opt e l =
  try Some (List.assoc e l) with Not_found -> None

let rec subst_ty s t =
  match t with
  | TInt -> t
  | TBool -> t
  | TString -> t
  | TPair (t_1, t_2) -> TPair (subst_ty s t_1, subst_ty s t_2)
  | TLoc t_0 -> TLoc (subst_ty s t_0)
  | TFun (t_1, t_2) -> TFun (subst_ty s t_1, subst_ty s t_2)
  | TVar t_var ->
    (match assoc_opt t_var s with
    | Some s_t -> s_t
    | None -> t)
  | TEqual t_var ->
    (match assoc_opt t_var s with
    | Some s_t -> s_t
    | None -> t)
  | TWrite t_var ->
    (match assoc_opt t_var s with
    | Some s_t -> s_t
    | None -> t)

let rec subst_tyenv s tenv =
  match tenv with
  | [] -> []
  | (x, t) :: tenv_tl -> (x, subst_ty s t) :: subst_tyenv s tenv_tl

let rec subst_sub s1 s2 =
  match s2 with
  | [] -> []
  | (s2_var, s2_typ) :: s2_tl -> (s2_var, subst_ty s1 s2_typ) :: subst_sub s1 s2_tl

let union_sub s1 s2 = s1 @ (subst_sub s1 s2)

let rec occur var t =
  match t with
  | TPair (t_1, t_2) -> (occur var t_1) || (occur var t_2)
  | TLoc t_0 -> occur var t_0
  | TFun (t_1, t_2) -> (occur var t_1) || (occur var t_2)
  | TVar t_var -> if var = t_var then true else false
  | _ -> false

let occur_sub var t =
  if occur var t
  then
    match t with
    | TVar _ -> []
    | _ -> raise (M.TypeError "Impossible unification (TVar)")
  else [(var, t)]

let rec unify t1 t2 =
  match (t1, t2) with
  | (TInt, TInt) -> []
  | (TBool, TBool) -> []
  | (TString, TString) -> []
  | (TPair (t1_1, t1_2), TPair (t2_1, t2_2)) ->
    let s = unify t1_1 t2_1 in
    let s' = unify (subst_ty s t1_2) (subst_ty s t2_2) in
    union_sub s' s
  | (TLoc t1_0, TLoc t2_0) -> unify t1_0 t2_0
  | (TFun (t1_1, t1_2), TFun (t2_1, t2_2)) ->
    let s = unify t1_1 t2_1 in
    let s' = unify (subst_ty s t1_2) (subst_ty s t2_2) in
    union_sub s' s
  | (TVar t1_var, _) -> occur_sub t1_var t2
  | (_, TVar t2_var) -> occur_sub t2_var t1
  | (TEqual t1_var, _) ->
    (match t2 with
    | TInt -> [(t1_var, t2)]
    | TBool -> [(t1_var, t2)]
    | TString -> [(t1_var, t2)]
    | TLoc _ -> [(t1_var, t2)]
    | TEqual _ -> [(t1_var, t2)]
    | TWrite _ -> [(t1_var, t2)]
    | _ -> raise (M.TypeError "Impossible unification (TEqual)"))
  | (_, TEqual t2_var) ->
    (match t1 with
    | TInt -> [(t2_var, t1)]
    | TBool -> [(t2_var, t1)]
    | TString -> [(t2_var, t1)]
    | TLoc _ -> [(t2_var, t1)]
    | TEqual _ -> [(t2_var, t1)]
    | TWrite _ -> [(t2_var, t1)]
    | _ -> raise (M.TypeError "Impossible unification (TEqual)"))
  | (TWrite t1_var, _) ->
    (match t2 with
    | TInt -> [(t1_var, t2)]
    | TBool -> [(t1_var, t2)]
    | TString -> [(t1_var, t2)]
    | TWrite _ -> [(t1_var, t2)]
    | _ -> raise (M.TypeError "Impossible unification (TWrite)"))
  | (_, TWrite t2_var) ->
    (match t1 with
    | TInt -> [(t2_var, t1)]
    | TBool -> [(t2_var, t1)]
    | TString -> [(t2_var, t1)]
    | TWrite _ -> [(t2_var, t1)]
    | _ -> raise (M.TypeError "Impossible unification (TWrite)"))
  | _ -> raise (M.TypeError "Impossible unification (else)")

let rec online_m tenv exp t =
  match exp with
  | M.CONST (M.S _) -> unify t TString
  | M.CONST (M.N _) -> unify t TInt
  | M.CONST (M.B _) -> unify t TBool
  | M.VAR x ->
    (match assoc_opt x tenv with
    | Some x_t -> unify t x_t
    | None -> raise (M.TypeError "Unbound variable"))
  | M.FN (x, e) ->
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    let s = unify t (TFun (a1, a2)) in
    let s' = online_m ((x, subst_ty s a1) :: subst_tyenv s tenv) e (subst_ty s a2) in
    union_sub s' s
  | M.APP (e1, e2) ->
    let a = TVar (new_var ()) in
    let s = online_m tenv e1 (TFun (a, t)) in
    let s' = online_m (subst_tyenv s tenv) e2 (subst_ty s a) in
    union_sub s' s
  | M.LET (M.REC (fun_id, arg_id, e1), e2) ->
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    let s = online_m ((fun_id, TFun (a1, a2)) :: (arg_id, a1) :: tenv) e1 a2 in
    let s' = online_m ((fun_id, subst_ty s (TFun (a1, a2))) :: subst_tyenv s tenv) e2 (subst_ty s t) in
    union_sub s' s
  | M.LET (M.VAL (val_id, e1), e2) ->
    let a = TVar (new_var ()) in
    let s = online_m tenv e1 a in
    let s' = online_m ((val_id, subst_ty s a) :: subst_tyenv s tenv) e2 (subst_ty s t) in
    union_sub s' s
  | M.IF (e1, e2, e3) ->
    let s = online_m tenv e1 TBool in
    let s' = online_m (subst_tyenv s tenv) e2 (subst_ty s t) in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e3 (subst_ty ss t) in
    union_sub s'' ss
  | M.BOP (M.ADD, e1, e2) ->
    let s = unify t TInt in
    let s' = online_m (subst_tyenv s tenv) e1 TInt in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e2 TInt in
    union_sub s'' ss
  | M.BOP (M.SUB, e1, e2) ->
    let s = unify t TInt in
    let s' = online_m (subst_tyenv s tenv) e1 TInt in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e2 TInt in
    union_sub s'' ss
  | M.BOP (M.EQ, e1, e2) ->
    let a = TEqual (new_var ()) in
    let s = unify t TBool in
    let s' = online_m (subst_tyenv s tenv) e1 (subst_ty s a) in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e2 (subst_ty ss a) in
    union_sub s'' ss
  | M.BOP (M.AND, e1, e2) ->
    let s = unify t TBool in
    let s' = online_m (subst_tyenv s tenv) e1 TBool in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e2 TBool in
    union_sub s'' ss
  | M.BOP (M.OR, e1, e2) ->
    let s = unify t TBool in
    let s' = online_m (subst_tyenv s tenv) e1 TBool in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e2 TBool in
    union_sub s'' ss
  | M.READ -> unify t TInt
  | M.WRITE e ->
    let a = TWrite (new_var ()) in
    let s = unify t a in
    let s' = online_m (subst_tyenv s tenv) e (subst_ty s t) in
    union_sub s' s
  | M.MALLOC e ->
    let a = TVar (new_var ()) in
    let s = unify t (TLoc a) in
    let s' = online_m (subst_tyenv s tenv) e (subst_ty s a) in
    union_sub s' s
  | M.ASSIGN (e1, e2) ->
    let s = online_m tenv e1 (TLoc t) in
    let s' = online_m (subst_tyenv s tenv) e2 (subst_ty s t) in
    union_sub s' s
  | M.BANG e -> online_m tenv e (TLoc t)
  | M.SEQ (e1, e2) ->
    let a = TVar (new_var ()) in
    let s = online_m tenv e1 a in
    let s' = online_m (subst_tyenv s tenv) e2 (subst_ty s t) in
    union_sub s' s
  | M.PAIR (e1, e2) ->
    let a1 = TVar (new_var ()) in
    let a2 = TVar (new_var ()) in
    let s = unify t (TPair (a1, a2)) in
    let s' = online_m (subst_tyenv s tenv) e1 (subst_ty s a1) in
    let ss = union_sub s' s in
    let s'' = online_m (subst_tyenv ss tenv) e2 (subst_ty ss a2) in
    union_sub s'' ss
  | M.FST e ->
    let a = TVar (new_var ()) in
    online_m tenv e (TPair (t, a))
  | M.SND e ->
    let a = TVar (new_var ()) in
    online_m tenv e (TPair (a, t))

let rec tyvar_to_ty t =
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t_1, t_2) -> M.TyPair (tyvar_to_ty t_1, tyvar_to_ty t_2)
  | TLoc t_0 -> M.TyLoc (tyvar_to_ty t_0)
  | TFun (t_1, t_2) -> M.TyArrow (tyvar_to_ty t_1, tyvar_to_ty t_2)
  | _ -> raise (M.TypeError "Polymorphic (non-self)")

let check : M.exp -> M.types = fun exp ->
  let a = new_var () in
  match assoc_opt a (online_m [] exp (TVar a)) with
  | Some t -> tyvar_to_ty t
  | None -> raise (M.TypeError "Polymorphic (self)")
