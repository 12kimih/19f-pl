type team =
  | Korea
  | France
  | USA
  | Brazil
  | Japan
  | Nigeria
  | Cameroon
  | Poland
  | Portugal
  | Italy
  | Germany
  | Norway
  | Sweden
  | England
  | Argentina

type tourna =
  | LEAF of team
  | NODE of tourna * tourna

let string_of_team t =
  match t with
  | Korea -> "Korea"
  | France -> "France"
  | USA -> "USA"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize t =
  match t with
  | LEAF l -> string_of_team l
  | NODE (nl, nr) -> "(" ^ parenize nl ^ " " ^ parenize nr ^ ")"

(* test *)
let () = print_endline (parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)))