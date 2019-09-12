type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add n1 n2 =
  match n1 with
  | NIL -> n2
  | ZERO tl1 ->
    (match n2 with
    | NIL -> n1
    | ZERO tl2 -> ZERO (crazy2add tl1 tl2) 
    | ONE tl2 -> ONE (crazy2add tl1 tl2)
    | MONE tl2 -> MONE (crazy2add tl1 tl2))
  | ONE tl1 ->
    (match n2 with
    | NIL -> n1
    | ZERO tl2 -> ONE (crazy2add tl1 tl2)
    | ONE tl2 -> ZERO (crazy2add (ONE NIL) (crazy2add tl1 tl2))
    | MONE tl2 -> ZERO (crazy2add tl1 tl2))
  | MONE tl1 ->
    (match n2 with
    | NIL -> n1
    | ZERO tl2 -> MONE (crazy2add tl1 tl2)
    | ONE tl2 -> ZERO (crazy2add tl1 tl2)
    | MONE tl2 -> ZERO (crazy2add (MONE NIL) (crazy2add tl1 tl2)))

(* test *)
let rec crazy2append n1 n2 =
  match n1 with
  | NIL -> n2
  | ZERO n1_tl -> ZERO (crazy2append n1_tl n2)
  | ONE n1_tl -> ONE (crazy2append n1_tl n2)
  | MONE n1_tl -> MONE (crazy2append n1_tl n2)

let rec crazy2reverse n =
  match n with
  | NIL -> NIL
  | ZERO tl -> crazy2append (crazy2reverse tl) (ZERO NIL)
  | ONE tl -> crazy2append (crazy2reverse tl) (ONE NIL)
  | MONE tl -> crazy2append (crazy2reverse tl) (MONE NIL)

let rec crazy2trim n =
  match n with
  | NIL -> ZERO NIL
  | ZERO tl -> crazy2trim tl
  | ONE tl -> n
  | MONE tl -> n

(*
let crazy2add n1 n2 =
  let rec crazy2add n1 n2 =
    match n1 with
    | NIL -> n2
    | ZERO tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> crazy2append (ZERO NIL) (crazy2add tl1 tl2) 
      | ONE tl2 -> crazy2append (ONE NIL) (crazy2add tl1 tl2)
      | MONE tl2 -> crazy2append (MONE NIL) (crazy2add tl1 tl2))
    | ONE tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> crazy2append (ONE NIL) (crazy2add tl1 tl2)
      | ONE tl2 -> crazy2append (ZERO NIL) (crazy2add (ONE NIL) (crazy2add tl1 tl2))
      | MONE tl2 -> crazy2append (ZERO NIL) (crazy2add tl1 tl2))
    | MONE tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> crazy2append (MONE NIL) (crazy2add tl1 tl2)
      | ONE tl2 -> crazy2append (ZERO NIL) (crazy2add tl1 tl2)
      | MONE tl2 -> crazy2append (ZERO NIL) (crazy2add (MONE NIL) (crazy2add tl1 tl2)))
  in crazy2trim (crazy2reverse (crazy2add (crazy2reverse n1) (crazy2reverse n2)))
*)

let crazy2trim_rev n = crazy2reverse (crazy2trim (crazy2reverse n))

let rec crazy2val n =
  match n with
  | NIL -> 0
  | ZERO tl -> 2 * crazy2val tl
  | ONE tl -> 2 * crazy2val tl + 1
  | MONE tl -> 2 * crazy2val tl - 1

(*
(* crazy2val in this code is meant to be for the real order of binary numbers *)
let rec crazy2repr n =
  if n = 0 then NIL else
  if (n mod 2) = 1 then crazy2append (crazy2repr (n / 2)) (ONE NIL) else
  if (n mod 2) = 0 then crazy2append (crazy2repr (n / 2)) (ZERO NIL) else crazy2append (crazy2repr (n / 2)) (MONE NIL)

let crazy2add n1 n2 = crazy2repr (crazy2val n1 + crazy2val n2)
*)

let n1 = ONE(MONE(ONE(ZERO(ONE NIL))))
let n2 = ONE(ONE(MONE(ONE(MONE NIL))))

let rec crazy2show n =
  match n with
  | NIL -> "NIL"
  | ZERO tl -> "ZERO(" ^ (crazy2show tl) ^ ")"
  | ONE tl -> "ONE(" ^ (crazy2show tl) ^ ")"
  | MONE tl -> "MONE(" ^ (crazy2show tl) ^ ")"

let () = print_endline (crazy2show (crazy2trim_rev (crazy2add n1 n2)))
let () = print_endline (string_of_int (crazy2val (crazy2add n1 n2)))
let () = print_endline ("Expected value: " ^ string_of_int (crazy2val n1 + crazy2val n2))