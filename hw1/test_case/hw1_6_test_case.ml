(* test *)
let rec crazy2trim n =
  match n with
  | NIL -> ZERO NIL
  | ZERO tl -> crazy2trim tl
  | ONE tl -> n
  | MONE tl -> n

let n1 = ONE(MONE(ZERO NIL))
let n2 = MONE(ONE(ONE(MONE(MONE NIL))))

let rec crazy2show n =
  match n with
  | NIL -> "NIL"
  | ZERO tl -> "ZERO(" ^ (crazy2show tl) ^ ")"
  | ONE tl -> "ONE(" ^ (crazy2show tl) ^ ")"
  | MONE tl -> "MONE(" ^ (crazy2show tl) ^ ")"

let () = print_endline (crazy2show (crazy2trim (crazy2add n1 n2)))
let () = print_endline (string_of_int (crazy2val (crazy2add n1 n2)))
let () = print_endline ("Expected value: " ^ string_of_int (crazy2val n1 + crazy2val n2))