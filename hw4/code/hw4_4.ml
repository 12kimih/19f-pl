type treasure =
  | StarBox
  | NameBox of string
type key =
  | Bar
  | Node of key * key
type map =
  | End of treasure
  | Branch of map * map
  | Guide of string * map

exception IMPOSSIBLE

(*
kl = key list
vkey = virtual key
vkl = virtual key list
nvkl = name virtual key list
*)

type virtualkey =
  | VirtualBar
  | VirtualNum of int
  | VirtualNode of virtualkey * virtualkey

let vkey_number = ref 0
let vkey_generate () =
  let x = !vkey_number in
  vkey_number := !vkey_number + 1;
  VirtualNum x

let rec find_vkey_by_name nvkl str =
  match nvkl with
  | [] -> None
  | (name, vkey) :: tl ->
    if name = str
    then Some vkey
    else find_vkey_by_name tl str

let get_vkey_by_name nvkl str =
  let vkey_option = find_vkey_by_name nvkl str in
  match vkey_option with
  | None -> 
    let str_key = vkey_generate () in
    ((str, str_key) :: nvkl, str_key)
  | Some vkey -> (nvkl, vkey)

let vkl_parse vkl =
  match vkl with
  | [] -> failwith "FATAL ERROR"
  | hd :: tl -> (hd, tl)

let rec vkey_find vkey vnum =
  match vkey with
  | VirtualBar -> false
  | VirtualNum vkey_num ->
    if vkey_num = vnum
    then true
    else false
  | VirtualNode (vkey_1, vkey_2) ->
    if vkey_find vkey_1 vnum
    then true
    else (
      if vkey_find vkey_2 vnum
      then true
      else false
    )

(* substitute vkey_to into vkey_from *)
let rec vkey_substitute vkey vkey_from vkey_to =
  match vkey with
  | VirtualBar -> vkey
  | VirtualNum vkey_num ->
    if vkey_num = vkey_from
    then vkey_to
    else vkey
  | VirtualNode (vkey_1, vkey_2) ->
    let vkey_1_sub = vkey_substitute vkey_1 vkey_from vkey_to in
    let vkey_2_sub = vkey_substitute vkey_2 vkey_from vkey_to in
    VirtualNode (vkey_1_sub, vkey_2_sub)

let rec nvkl_substitute nvkl vkey_from vkey_to =
  match nvkl with
  | [] -> nvkl
  | (name, vkey) :: nvkl_tl ->
    let nvkl_tl_sub = nvkl_substitute nvkl_tl vkey_from vkey_to in
    let vkey_sub = vkey_substitute vkey vkey_from vkey_to in
    (name, vkey_sub) :: nvkl_tl_sub

let rec vkl_substitute vkl vkey_from vkey_to =
  match vkl with
  | [] -> vkl
  | vkey :: vkl_tl ->
    let vkl_tl_sub = vkl_substitute vkl_tl vkey_from vkey_to in
    let vkey_sub = vkey_substitute vkey vkey_from vkey_to in
    vkey_sub :: vkl_tl_sub

let nvkl_vkl_substitute nvkl vkl vkey_from vkey_to =
  let nvkl1 = nvkl_substitute nvkl vkey_from vkey_to in
  let vkl1 = vkl_substitute vkl vkey_from vkey_to in
  (nvkl1, vkl1)

(* compare vkey1 and vkey2, and put a specified key into a VirtualNum key *)
let rec vkey_compare nvkl vkl vkey1 vkey2 =
  match vkey1 with
  | VirtualBar ->
    (match vkey2 with
      | VirtualBar -> (nvkl, vkl)
      | VirtualNum vkey2_num -> nvkl_vkl_substitute nvkl vkl vkey2_num vkey1
      | VirtualNode (_, _) -> raise IMPOSSIBLE)
  | VirtualNum vkey1_num -> 
    (match vkey2 with
    | VirtualBar -> nvkl_vkl_substitute nvkl vkl vkey1_num vkey2
    | VirtualNum vkey2_num ->
      if vkey1_num = vkey2_num
      then (nvkl, vkl)
      else (
        if vkey1_num < vkey2_num
        then nvkl_vkl_substitute nvkl vkl vkey2_num vkey1
        else nvkl_vkl_substitute nvkl vkl vkey1_num vkey2
      )
    | VirtualNode (_, _) ->
      if vkey_find vkey2 vkey1_num
      then raise IMPOSSIBLE
      else nvkl_vkl_substitute nvkl vkl vkey1_num vkey2)
  | VirtualNode (vkey1_1, vkey1_2) ->
    (match vkey2 with
    | VirtualBar -> raise IMPOSSIBLE
    | VirtualNum vkey2_num ->
      if vkey_find vkey1 vkey2_num
      then raise IMPOSSIBLE
      else nvkl_vkl_substitute nvkl vkl vkey2_num vkey1
    | VirtualNode (vkey2_1, vkey2_2) ->
      let (nvkl1, vkl1) = vkey_compare nvkl (vkey1_2 :: vkey2_2 :: vkl) vkey1_1 vkey2_1 in
      let (vkey1_2, vkl1) = vkl_parse vkl1 in
      let (vkey2_2, vkl1) = vkl_parse vkl1 in
      vkey_compare nvkl1 vkl1 vkey1_2 vkey2_2)

let rec map_to_vkey nvkl vkl map =
  match map with
  | End treasure ->
    (match treasure with
    | StarBox ->
      let vkey_option = find_vkey_by_name nvkl "*" in
      (match vkey_option with
      | None -> 
        (("*", VirtualBar) :: nvkl, vkl, VirtualBar)
      | Some vkey -> (nvkl, vkl, vkey))
    | NameBox str ->
      let (nvkl, vkey) = get_vkey_by_name nvkl str in
      (nvkl, vkl, vkey))
  | Branch (map1, map2) ->
    let (nvkl1, vkl1, vkey1) = map_to_vkey nvkl vkl map1 in
    let (nvkl2, vkl2, vkey2) = map_to_vkey nvkl1 (vkey1 :: vkl1) map2 in
    let (vkey1, vkl2) = vkl_parse vkl2 in
    (match vkey1 with
    | VirtualBar -> raise IMPOSSIBLE
    | VirtualNum vkey1_num ->
      let vkey1_1 = vkey_generate () in
      let vkey1_2 = vkey_generate () in
      let vkey1 = VirtualNode (vkey1_1, vkey1_2) in
      let (nvkl3, vkl3) = nvkl_vkl_substitute nvkl2 (vkey2 :: vkl2) vkey1_num vkey1 in
      let (vkey2, vkl3) = vkl_parse vkl3 in
      let (nvkl4, vkl4) = vkey_compare nvkl3 vkl3 vkey1_1 vkey2 in
      (nvkl4, vkl4, vkey1_2)
    | VirtualNode (vkey1_1, vkey1_2) ->
      let (nvkl3, vkl3) = vkey_compare nvkl2 (vkey1_2 :: vkl2) vkey1_1 vkey2 in
      let (vkey1_2, vkl3) = vkl_parse vkl3 in
      (nvkl3, vkl3, vkey1_2))
  | Guide (str, map1) ->
    let (nvkl, vkey) = get_vkey_by_name nvkl str in
    let (nvkl1, vkl1, vkey1) = map_to_vkey nvkl (vkey :: vkl) map1 in
    let (vkey, vkl1) = vkl_parse vkl1 in
    (nvkl1, vkl1, VirtualNode(vkey, vkey1))

let rec vkey_to_key vkey =
  match vkey with
  | VirtualBar -> Bar
  | VirtualNum _ -> Bar
  | VirtualNode (vkey_1, vkey_2) ->
    let key_1 = vkey_to_key vkey_1 in
    let key_2 = vkey_to_key vkey_2 in
    Node (key_1, key_2)

let rec vkl_to_kl vkl =
  match vkl with
  | [] -> []
  | hd :: tl -> vkey_to_key hd :: vkl_to_kl tl

let rec kl_unique kl =
  match kl with
  | [] -> []
  | hd :: tl -> hd :: kl_unique (List.filter (fun x -> x <> hd) tl)

let getReady map =
  vkey_number := 0;
  let (nvkl, _, _) = map_to_vkey [] [] map in
  let (_, vkl) = List.split nvkl in
  kl_unique (vkl_to_kl vkl)

(* test *)

let e1 = End (NameBox "x")
let e2 = Guide ("x", e1)
let e3 = Branch (e2, End StarBox)
let e4 = Branch (Guide ("x", Branch (e1, e1)), End StarBox)
let e5_1 = End (NameBox "y")
let e5_2 = Guide ("y", e5_1)
let e5 = Branch (e2, Branch (e5_2, End StarBox))
let e6 = Branch (e2, e5_2)
let e7 = Branch (e1, End StarBox)

let getReady_exception map =
  try getReady map
  with IMPOSSIBLE -> []

let res1 = getReady_exception e1
let res2 = getReady_exception e2
let res3 = getReady_exception e3
let res4 = getReady_exception e4
let res5 = getReady_exception e5
let res6 = getReady_exception e6
let res7 = getReady_exception e7
