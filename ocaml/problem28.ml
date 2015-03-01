let insert cmp v list =
  let rec aux acc v = function
  | [] -> List.rev ( v::acc )
  | [x] ->  if 0 < ( cmp x v ) then List.rev ( v::acc ) @ [x] else aux ( x::acc ) v []
  | h::t as l -> if 0 < ( cmp h v ) then List.rev( v::acc ) @ l else aux ( h::acc ) v t in
  aux [] v list;;

let length_cmp lh rh = 
  let lhl = List.length lh in
  let rhl = List.length rh in
  if lhl < rhl then -1 else if rhl < lhl then 1 else  0;;

let length_sort list =
  let rec aux acc = function
  | [] -> acc
  | [x] -> insert length_cmp x acc
  | h::t  -> aux ( insert length_cmp h acc ) t in
  aux [] list;;

let rec lookup key = function
| [] -> None
| (k,v)::t -> if key = k then Some v else lookup key t;;

let countup key tab =
  let rec aux acc key = function
  | [] -> List.rev ( ( key, 1 ) :: acc )
  | ((k,v) as e)::t -> if key = k then ( List.rev ( (k,v+1)::acc) ) @ t else aux ( e::acc ) key t in
  aux [] key tab;;

let count list =
  let rec aux acc = function
  | [] -> acc
  | [x] -> countup ( List.length x ) acc
  | h::t -> aux ( countup (List.length h ) acc ) t in
  aux [] list;;

let count_cmp table lh rh =
  let clh = lookup ( List.length lh ) table and crh = lookup ( List.length rh ) table in
  match ( clh, crh ) with
  | ( None , _ ) | ( _ , None ) | ( None, None ) -> 0
  | ( Some c1, Some c2 ) -> if c1 < c2 then -1 else if c2 < c1 then 1 else 0;;

let frequency_sort list =
  let binded_count_cmp = count_cmp ( count list ) in
  let rec aux acc l = match l with
  | [] -> acc
  | [x] -> insert binded_count_cmp x acc
  | h::t -> aux ( insert binded_count_cmp h acc ) t in
  aux [] list;;

length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"]; ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"]; ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
