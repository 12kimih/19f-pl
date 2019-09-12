type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val n =
  match n with
  | NIL -> 0
  | ZERO tl -> 2 * crazy2val tl
  | ONE tl -> 2 * crazy2val tl + 1
  | MONE tl -> 2 * crazy2val tl - 1