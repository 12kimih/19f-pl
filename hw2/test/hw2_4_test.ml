module type Queue =
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

module IntListQ =
struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ : queue = ([], [])
  let enQ (((s1, s2), e) : queue * element) : queue = (e :: s1, s2)
  let deQ ((s1, s2) : queue) : element * queue =
    let rec s1tos2 (s1, s2) =
      match s1 with
      | [] -> (s1, s2)
      | hd :: tl -> s1tos2 (tl, hd :: s2)
    in
    match s2 with
    | [] ->
      let (s1_new, s2_new) = s1tos2 (s1, s2) in
      ( match s2_new with
        | [] -> raise EMPTY_Q
        | hd :: tl -> (hd, (s1_new, tl)) )
    | hd::tl -> (hd, (s1, tl))
end

(* test *)

(*
(* implementation using refs(mutable variables) *)
module IntListQ = struct
  type element = int list
  type stack = element list ref
  type queue = stack * stack
  exception EMPTY_Q
  let emptyQ : queue = (ref [], ref [])
  let enQ (((s1, s2), e) : queue * element) : queue = s1 := e :: !s1; (s1, s2)
  let deQ ((s1, s2) : queue) : element * queue =
    let rec s1tos2 (s1, s2) =
      match !s1 with
      | [] -> ()
      | hd :: tl ->
        s1 := tl;
        s2 := hd :: !s2;
        s1tos2 (s1, s2)
    in
    match !s2 with
    | [] ->
      s1tos2 (s1, s2);
      ( match !s2 with
        | [] -> raise EMPTY_Q
        | hd :: tl -> s2 := tl; (hd, (s1, s2)) )
    | hd::tl -> s2 := tl; (hd, (s1, s2))
end
*)

module ValidIntListQ = (IntListQ : Queue)

let rec reverse_list =
  function
  | [] -> []
  | hd :: tl -> reverse_list tl @ [hd]

let rec print_element =
  let rec without_paren =
    function
    | [] -> ()
    | hd :: tl ->
      ( match tl with
        | [] ->
          print_int hd
        | tl_hd :: tl_tl ->
          print_int hd;
          print_char ' ';
          without_paren tl )
  in
  function
  | [] -> print_string "[]"
  | hd :: tl ->
    ( match tl with
      | [] ->
        print_char '[';
        print_int hd;
        print_char ']'
      | tl_hd :: tl_tl ->
        print_char '[';
        print_int hd;
        print_char ' ';
        without_paren tl;
        print_char ']' )

let rec print_stack =
  function
  | [] -> ()
  | hd :: tl ->
    ( match tl with
      | [] ->
        print_element hd
      | tl_hd :: tl_tl ->
        print_element hd;
        print_char ' ';
        print_stack tl )

let printQ (s1, s2) =
  match s1 with
  | [] ->
    ( match s2 with
      | [] -> print_endline "Empty Queue"
      | hd2 :: tl2 ->
        print_stack (reverse_list s2);
        print_char '\n' )
  | hd1 :: tl1 ->
    ( match s2 with
      | [] ->
        print_stack s1;
        print_char '\n'
      | hd2 :: tl2 ->
        print_stack s1;
        print_char ' ';
        print_stack (reverse_list s2);
        print_char '\n' )

(*
let myQ = IntListQ.emptyQ
let yourQ1 = IntListQ.enQ(myQ, [1; 2; 3])
let yourQ2 = IntListQ.enQ(yourQ1, [2; 3; 4])
let yourQ3 = IntListQ.enQ(yourQ2, [3; 4; 5])
let (x, restQ) = IntListQ.deQ yourQ3
let hisQ = IntListQ.enQ(restQ, [4; 5; 6])

let () = printQ myQ
let () = printQ yourQ1
let () = printQ yourQ2
let () = printQ yourQ3
let () = print_element x
let () = print_char '\n'
let () = printQ restQ
let () = printQ hisQ
*)

let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ (myQ, [1])
let (x, restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ (myQ, [2])

let q1 = IntListQ.enQ (myQ, [1; 2; 3; 4; 5])
let q2 = IntListQ.enQ (q1, [1; 2; 3])
let q3 = IntListQ.enQ (q2, [1; 2])

let e4, q4 = IntListQ.deQ q3
let e5, q5 = IntListQ.deQ q4
let e6, q6 = IntListQ.deQ q5

let q7 = IntListQ.enQ (q6, [1; 3; 5])
let q8 = IntListQ.enQ (q7, [2; 4; 6])

let e9, q9 = IntListQ.deQ q8

let q10 = IntListQ.enQ (q9, [9; 8])

let e11, q11 = IntListQ.deQ q10

let q12 = IntListQ.enQ (q11, [7])
let q13 = IntListQ.enQ (q12, [8])

let e14, q14 = IntListQ.deQ q13
let e15, q15 = IntListQ.deQ q14
let e16, q16 = IntListQ.deQ q15