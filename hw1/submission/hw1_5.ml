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

let crazy2val n =
  let rec crazy2val_rev n =
    match n with
    | NIL -> 0
    | ZERO tl -> 2 * crazy2val_rev tl
    | ONE tl -> 2 * crazy2val_rev tl + 1
    | MONE tl -> 2 * crazy2val_rev tl - 1
  in crazy2val_rev (crazy2reverse n)