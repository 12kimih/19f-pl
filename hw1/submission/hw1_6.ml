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