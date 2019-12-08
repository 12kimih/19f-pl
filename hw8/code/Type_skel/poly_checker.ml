(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open M

type var =
  | G of string
  | E of string
  | W of string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

let copy_var = fun v ->
  match v with
  | G _ -> G (new_var ())
  | E _ -> E (new_var ())
  | W _ -> W (new_var ())

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp (ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map copy_var alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let assoc_opt = fun a l ->
  try Some (List.assoc a l) with Not_found -> None

let rec occur : var -> typ -> bool = fun x t ->
  match t with
  | TPair (t1, t2) | TFun (t1, t2) -> (occur x t1) || (occur x t2)
  | TLoc t' -> occur x t'
  | TVar x' -> if x = x' then true else false
  | _ -> false

let rec unify : typ -> typ -> subst = fun t1 t2 ->
  match (t1, t2) with
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst
  | (TPair (t1', t1''), TPair (t2', t2'')) | (TFun (t1', t1''), TFun (t2', t2'')) ->
    let s = unify t1' t2' in
    let s' = unify (s t1'') (s t2'') in
    s' @@ s
  | (TLoc t1', TLoc t2') -> unify t1' t2'
  | (TVar (G s), t) | (t, TVar (G s)) ->
    if (occur (G s) t) && (t <> TVar (G s))
    then raise (M.TypeError "unification failed: occurred")
    else make_subst (G s) t
  | (TVar (E s), t) | (t, TVar (E s)) ->
    if (occur (E s) t) && (t <> TVar (E s))
    then raise (M.TypeError "unification failed: occurred")
    else
      (match t with
      | TInt | TBool | TString | TLoc _ | TVar (E _) | TVar (W _) -> make_subst (E s) t
      | _ -> raise (M.TypeError "unification failed: equal type"))
  | (TVar (W s), t) | (t, TVar (W s)) ->
    (match t with
    | TInt | TBool | TString | TVar (W _) -> make_subst (W s) t
    | _ -> raise (M.TypeError "unification failed: write type"))
  | _ -> raise (M.TypeError "unfication failed: unmatched")

let rec expansive = fun exp ->
  match exp with
  | M.CONST _ -> false
  | M.VAR _ -> false
  | M.FN _ -> false
  | M.APP _ -> true
  | M.LET (M.REC _, e2) -> expansive e2
  | M.LET (M.VAL (_, e1), e2) -> (expansive e1) || (expansive e2) 
  | M.IF (e1, e2, e3) -> (expansive e1) || (expansive e2) || (expansive e3)
  | M.BOP (_, e1, e2) -> (expansive e1) || (expansive e2)
  | M.READ -> false
  | M.WRITE e -> expansive e
  | M.MALLOC _ -> true
  | M.ASSIGN (e1, e2) -> (expansive e1) || (expansive e2)
  | M.BANG e -> expansive e
  | M.SEQ (e1, e2) -> (expansive e1) || (expansive e2)
  | M.PAIR (e1, e2) -> (expansive e1) || (expansive e2)
  | M.FST e -> expansive e
  | M.SND e -> expansive e

let rec m_inference : typ_env -> M.exp -> typ -> subst = fun tyenv exp t ->
  match exp with
  | M.CONST (M.S _) -> unify t TString
  | M.CONST (M.N _) -> unify t TInt
  | M.CONST (M.B _) -> unify t TBool
  | M.VAR x ->
    (match assoc_opt x tyenv with
    | Some (SimpleTyp t') -> unify t t'
    | Some (GenTyp (alphas, t')) ->
      let betas = List.map (fun alpha -> TVar (copy_var alpha)) alphas in
      let s =
        List.fold_left2
          (fun acc_subst alpha beta -> make_subst alpha beta @@ acc_subst)
          empty_subst alphas betas
      in unify t (s t')
    | None -> raise (M.TypeError "unbound variable"))
  | M.FN (x, e) ->
    let a1 = TVar (G (new_var ())) in
    let a2 = TVar (G (new_var ())) in
    let s = unify t (TFun (a1, a2)) in
    let s' = m_inference ((x, SimpleTyp (s a1)) :: subst_env s tyenv) e (s a2) in
    s' @@ s
  | M.APP (e1, e2) ->
    let a = TVar (G (new_var ())) in
    let s = m_inference tyenv e1 (TFun (a, t)) in
    let s' = m_inference (subst_env s tyenv) e2 (s a) in
    s' @@ s
  | M.LET (M.REC (f, x, e1), e2) ->
    let a1 = TVar (G (new_var ())) in
    let a2 = TVar (G (new_var ())) in
    let s = m_inference ((f, SimpleTyp (TFun (a1, a2))) :: (x, SimpleTyp a1) :: tyenv) e1 a2 in
    let tyenv' = subst_env s tyenv in
    let s' = m_inference ((f, generalize tyenv' (s (TFun (a1, a2)))) :: tyenv') e2 (s t) in
    s' @@ s
  | M.LET (M.VAL (x, e1), e2) ->
    let a = TVar (G (new_var ())) in
    let s = m_inference tyenv e1 a in
    let tyenv' = subst_env s tyenv in
    if expansive e1
    then
      let s' = m_inference ((x, SimpleTyp (s a)) :: tyenv') e2 (s t) in
      s' @@ s
    else
      let s' = m_inference ((x, generalize tyenv' (s a)) :: tyenv') e2 (s t) in
      s' @@ s
  | M.IF (e1, e2, e3) ->
    let s = m_inference tyenv e1 TBool in
    let s' = m_inference (subst_env s tyenv) e2 (s t) in
    let ss = s' @@ s in
    let s'' = m_inference (subst_env ss tyenv) e3 (ss t) in
    s'' @@ ss
  | M.BOP (M.ADD, e1, e2) | M.BOP (M.SUB, e1, e2) ->
    let s = unify t TInt in
    let s' = m_inference (subst_env s tyenv) e1 TInt in
    let ss = s' @@ s in
    let s'' = m_inference (subst_env ss tyenv) e2 TInt in
    s'' @@ ss
  | M.BOP (M.EQ, e1, e2) ->
    let a = TVar (E (new_var ())) in
    let s = unify t TBool in
    let s' = m_inference (subst_env s tyenv) e1 (s a) in
    let ss = s' @@ s in
    let s'' = m_inference (subst_env ss tyenv) e2 (ss a) in
    s'' @@ ss
  | M.BOP (M.AND, e1, e2) | M.BOP (M.OR, e1, e2) ->
    let s = unify t TBool in
    let s' = m_inference (subst_env s tyenv) e1 TBool in
    let ss = s' @@ s in
    let s'' = m_inference (subst_env ss tyenv) e2 TBool in
    s'' @@ ss
  | M.READ -> unify t TInt
  | M.WRITE e ->
    let a = TVar (W (new_var ())) in
    let s = unify t a in
    let s' = m_inference (subst_env s tyenv) e (s t) in
    s' @@ s
  | M.MALLOC e ->
    let a = TVar (G (new_var ())) in
    let s = unify t (TLoc a) in
    let s' = m_inference (subst_env s tyenv) e (s a) in
    s' @@ s
  | M.ASSIGN (e1, e2) ->
    let s = m_inference tyenv e1 (TLoc t) in
    let s' = m_inference (subst_env s tyenv) e2 (s t) in
    s' @@ s
  | M.BANG e -> m_inference tyenv e (TLoc t)
  | M.SEQ (e1, e2) ->
    let a = TVar (G (new_var ())) in
    let s = m_inference tyenv e1 a in
    let s' = m_inference (subst_env s tyenv) e2 (s t) in
    s' @@ s
  | M.PAIR (e1, e2) ->
    let a1 = TVar (G (new_var ())) in
    let a2 = TVar (G (new_var ())) in
    let s = unify t (TPair (a1, a2)) in
    let s' = m_inference (subst_env s tyenv) e1 (s a1) in
    let ss = s' @@ s in
    let s'' = m_inference (subst_env ss tyenv) e2 (ss a2) in
    s'' @@ ss
  | M.FST e ->
    let a = TVar (G (new_var ())) in
    m_inference tyenv e (TPair (t, a))
  | M.SND e ->
    let a = TVar (G (new_var ())) in
    m_inference tyenv e (TPair (a, t))

let rec type_of = fun t ->
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (type_of t1, type_of t2)
  | TLoc t' -> M.TyLoc (type_of t')
  | TFun (t_1, t_2) -> raise (M.TypeError "function type")
  | _ -> raise (M.TypeError "unknown type")

let check : M.exp -> M.typ = fun exp ->
  let a = TVar (G (new_var ())) in
  type_of ((m_inference [] exp a) a)
