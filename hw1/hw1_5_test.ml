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

(* test *)
let test_c = MONE(MONE(MONE NIL))
let () = print_endline (string_of_int (crazy2val test_c))