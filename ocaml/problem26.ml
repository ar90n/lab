(*
let rec concat_all acc c = function
| [] -> acc
| h::t -> concat_all ((c::h)::acc) c t;;

concat_all [] "c" [ ["a"]; ["b";"c"]];;

let extract n list =
let rec aux n len list = if len < n then [] else
( match list with
| [] -> raise Not_found
| [x] as l -> [l]
| h::t when n = 1 -> ( aux n ( len - 1 ) t ) @ [[h]]
| h::t -> ( concat_all [] h ( aux ( n - 1 ) ( len - 1 ) t ) ) @ ( aux n ( len - 1 ) t ) ) in
let len = List.length list in
aux n len list;;
*)

let extract k list =
let rec aux k acc emit = function
| [] -> acc
| h::t -> if k = 1 then aux k ( emit [h] acc ) emit t else
let new_emit x = emit ( h :: x ) in
aux k ( aux ( k - 1 ) acc new_emit t ) emit t
in
let emit x acc = x :: acc in
aux k [] emit list;;

extract 2 ["a";"b";"c";"d"];;
