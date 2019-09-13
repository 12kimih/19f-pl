type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add (n1, n2) =
  match n1 with
  | NIL -> n2
  | ZERO tl1 ->
    (match n2 with
    | NIL -> n1
    | ZERO tl2 -> ZERO (crazy2add (tl1, tl2)) 
    | ONE tl2 -> ONE (crazy2add (tl1, tl2))
    | MONE tl2 -> MONE (crazy2add (tl1, tl2)))
  | ONE tl1 ->
    (match n2 with
    | NIL -> n1
    | ZERO tl2 -> ONE (crazy2add (tl1, tl2))
    | ONE tl2 -> ZERO (crazy2add (ONE NIL, crazy2add (tl1, tl2)))
    | MONE tl2 -> ZERO (crazy2add (tl1, tl2)))
  | MONE tl1 ->
    (match n2 with
    | NIL -> n1
    | ZERO tl2 -> MONE (crazy2add (tl1, tl2))
    | ONE tl2 -> ZERO (crazy2add (tl1, tl2))
    | MONE tl2 -> ZERO (crazy2add (MONE NIL, crazy2add (tl1, tl2))))