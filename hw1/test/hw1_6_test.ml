type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec append_crazy2 n1 n2 =
  match n1 with
  | NIL -> n2
  | ZERO n1_tl -> ZERO (append_crazy2 n1_tl n2)
  | ONE n1_tl -> ONE (append_crazy2 n1_tl n2)
  | MONE n1_tl -> MONE (append_crazy2 n1_tl n2)

let rec reverse_crazy2 n =
  match n with
  | NIL -> NIL
  | ZERO tl -> append_crazy2 (reverse_crazy2 tl) (ZERO NIL)
  | ONE tl -> append_crazy2 (reverse_crazy2 tl) (ONE NIL)
  | MONE tl -> append_crazy2 (reverse_crazy2 tl) (MONE NIL)

let rec trim_crazy2 n =
  match n with
  | NIL -> ZERO NIL
  | ZERO tl -> trim_crazy2 tl
  | ONE tl -> n
  | MONE tl -> n

let crazy2add n1 n2 =
  let rec crazy2add_rev n1 n2 =
    match n1 with
    | NIL -> n2
    | ZERO tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> append_crazy2 (ZERO NIL) (crazy2add_rev tl1 tl2) 
      | ONE tl2 -> append_crazy2 (ONE NIL) (crazy2add_rev tl1 tl2)
      | MONE tl2 -> append_crazy2 (MONE NIL) (crazy2add_rev tl1 tl2))
    | ONE tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> append_crazy2 (ONE NIL) (crazy2add_rev tl1 tl2)
      | ONE tl2 -> append_crazy2 (ZERO NIL) (crazy2add_rev (ONE NIL) (crazy2add_rev tl1 tl2))
      | MONE tl2 -> append_crazy2 (ZERO NIL) (crazy2add_rev tl1 tl2))
    | MONE tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> append_crazy2 (MONE NIL) (crazy2add_rev tl1 tl2)
      | ONE tl2 -> append_crazy2 (ZERO NIL) (crazy2add_rev tl1 tl2)
      | MONE tl2 -> append_crazy2 (ZERO NIL) (crazy2add_rev (MONE NIL) (crazy2add_rev tl1 tl2)))
  in trim_crazy2 (reverse_crazy2 (crazy2add_rev (reverse_crazy2 n1) (reverse_crazy2 n2)))

(* test *)
let crazy2val n =
  let rec crazy2val_rev n =
    match n with
    | NIL -> 0
    | ZERO tl -> 2 * crazy2val_rev tl
    | ONE tl -> 2 * crazy2val_rev tl + 1
    | MONE tl -> 2 * crazy2val_rev tl - 1
  in crazy2val_rev (reverse_crazy2 n)

(*
let rec crazy2repr n =
  if n = 0 then NIL else
  if (n mod 2) = 1 then append_crazy2 (crazy2repr (n / 2)) (ONE NIL) else
  if (n mod 2) = 0 then append_crazy2 (crazy2repr (n / 2)) (ZERO NIL) else append_crazy2 (crazy2repr (n / 2)) (MONE NIL)

let crazy2add n1 n2 = crazy2repr (crazy2val n1 + crazy2val n2)
*)

let n1 = MONE(MONE(ZERO NIL))
let n2 = ONE(MONE(ONE NIL))

let rec crazy2show n =
  match n with
  | NIL -> "NIL"
  | ZERO tl -> "ZERO(" ^ (crazy2show tl) ^ ")"
  | ONE tl -> "ONE(" ^ (crazy2show tl) ^ ")"
  | MONE tl -> "MONE(" ^ (crazy2show tl) ^ ")"

let () = print_endline (crazy2show (crazy2add n1 n2))
let () = print_endline (string_of_int (crazy2val (crazy2add n1 n2)))
let () = print_endline ("Expected value: " ^ string_of_int (crazy2val n1 + crazy2val n2))