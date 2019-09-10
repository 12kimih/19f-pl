type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val n =
  let rec len n =
    match n with
    | NIL -> 0
    | ZERO k -> 1 + len k
    | ONE k -> 1 + len k
    | MONE k -> 1 + len k
  in
  let rec power_of_2 n =
    if n > 0 then 2 * power_of_2 (n - 1) else 1
  in
  match n with
  | NIL -> 0
  | ZERO k -> crazy2val k
  | ONE k -> crazy2val k + power_of_2 (len k)
  | MONE k -> crazy2val k - power_of_2 (len k)

let rec append_crazy2 n1 n2 =
  match n1 with
  | NIL -> n2
  | ZERO n1_tl -> ZERO (append_crazy2 n1_tl n2)
  | ONE n1_tl -> ONE (append_crazy2 n1_tl n2)
  | MONE n1_tl -> MONE (append_crazy2 n1_tl n2)

let crazy2add n1 n2 =
  let rec crazy2repr n =
    if n = 0 then ZERO NIL else
    if (n mod 2) = 1 then append_crazy2 (crazy2repr (n / 2)) (ONE NIL) else
    if (n mod 2) = 0 then append_crazy2 (crazy2repr (n / 2)) (ZERO NIL) else append_crazy2 (crazy2repr (n / 2)) (MONE NIL)
  in
  let sum = crazy2val n1 + crazy2val n2 in crazy2repr sum

(* test *)
let n1 = ONE(MONE(ZERO NIL))
let n2 = MONE(MONE(ONE NIL))

let rec crazy2show n =
  match n with
  | NIL -> "NIL"
  | ZERO tl -> "ZERO(" ^ (crazy2show tl) ^ ")"
  | ONE tl -> "ONE(" ^ (crazy2show tl) ^ ")"
  | MONE tl -> "MONE(" ^ (crazy2show tl) ^ ")"

let () = print_endline (crazy2show (crazy2add n1 n2))
let () = print_endline (string_of_int (crazy2val (crazy2add n1 n2)))