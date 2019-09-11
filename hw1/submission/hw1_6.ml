type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

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

let crazy2add n1 n2 =
  let rec crazy2add_rev n1 n2 =
    match n1 with
    | NIL -> n2
    | ZERO tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> crazy2append (ZERO NIL) (crazy2add_rev tl1 tl2) 
      | ONE tl2 -> crazy2append (ONE NIL) (crazy2add_rev tl1 tl2)
      | MONE tl2 -> crazy2append (MONE NIL) (crazy2add_rev tl1 tl2))
    | ONE tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> crazy2append (ONE NIL) (crazy2add_rev tl1 tl2)
      | ONE tl2 -> crazy2append (ZERO NIL) (crazy2add_rev (ONE NIL) (crazy2add_rev tl1 tl2))
      | MONE tl2 -> crazy2append (ZERO NIL) (crazy2add_rev tl1 tl2))
    | MONE tl1 ->
      (match n2 with
      | NIL -> n1
      | ZERO tl2 -> crazy2append (MONE NIL) (crazy2add_rev tl1 tl2)
      | ONE tl2 -> crazy2append (ZERO NIL) (crazy2add_rev tl1 tl2)
      | MONE tl2 -> crazy2append (ZERO NIL) (crazy2add_rev (MONE NIL) (crazy2add_rev tl1 tl2)))
  in crazy2trim (crazy2reverse (crazy2add_rev (crazy2reverse n1) (crazy2reverse n2)))