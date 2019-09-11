type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

(*
let rec crazy2val n =
  let rec len n =
    match n with
    | NIL -> 0
    | ZERO tl -> 1 + len tl
    | ONE tl -> 1 + len tl
    | MONE tl -> 1 + len tl
  in
  let rec power_of_2 n =
    if n > 0 then 2 * power_of_2 (n - 1) else 1
  in
  match n with
  | NIL -> 0
  | ZERO tl -> crazy2val tl
  | ONE tl -> crazy2val tl + power_of_2 (len tl)
  | MONE tl -> crazy2val tl - power_of_2 (len tl)
*)

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

let crazy2val n =
  let rec crazy2val_rev n =
    match n with
    | NIL -> 0
    | ZERO tl -> 2 * crazy2val_rev tl
    | ONE tl -> 2 * crazy2val_rev tl + 1
    | MONE tl -> 2 * crazy2val_rev tl - 1
  in crazy2val_rev (reverse_crazy2 n)

(* test *)
let test_c = MONE(ONE(ZERO NIL))
let () = print_endline (string_of_int (crazy2val test_c))