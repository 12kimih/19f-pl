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
    | hd :: tl -> (hd, (s1, tl))
end