type heap =
  | EMPTY
  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h =
  match h with
  | EMPTY -> -1
  | NODE (r, _, _, _) -> r

let shake (x, lh, rh) =
  if (rank lh) >= (rank rh)
  then NODE (rank rh + 1, x, lh, rh)
  else NODE (rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
  match h1 with
  | EMPTY -> h2
  | NODE (_, x1, lh1, rh1) ->
    ( match h2 with
      | EMPTY -> h1
      | NODE (_, x2, lh2, rh2) ->
        if x1 <= x2
        then let rh_new = merge (rh1, h2) in shake (x1, lh1, rh_new)
        else let rh_new = merge (rh2, h1) in shake (x2, lh2, rh_new) )

let findMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, _, _) -> x

let insert (x, h) = merge (h, NODE (0, x, EMPTY, EMPTY))

let deleteMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge(lh, rh)

  (* test *)

let _ = merge (EMPTY, EMPTY)

let _ = merge (EMPTY, NODE (0, 17, EMPTY, EMPTY))

let _ = merge (NODE (0, 17, EMPTY, EMPTY), EMPTY)

let h1 = merge (NODE (0, 20, EMPTY, EMPTY), NODE (0, 50, EMPTY, EMPTY))

let h2 = merge (h1, NODE (0, 99, EMPTY, EMPTY))

let h3 = merge (NODE (0, 7, EMPTY, EMPTY), h2)

let h4 = merge (NODE (0, 25, EMPTY, EMPTY), h3)

let h5 = merge (NODE (0, 5, EMPTY, EMPTY), NODE (0, 10, EMPTY, EMPTY))

let h6 = merge (NODE (0, 15, EMPTY, EMPTY), h5)

let h7 = merge (h4, NODE (0, 1, EMPTY, EMPTY))

let h8 = merge (h7, h6)

let h9 = merge (NODE (0, 22, EMPTY, EMPTY), NODE (0, 75, EMPTY, EMPTY))

let h10 = merge (h9, h8)

let _ = deleteMin h10