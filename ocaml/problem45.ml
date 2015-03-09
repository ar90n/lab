type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let is_symmetric root =
let rec aux lh rh = match ( lh, rh ) with
| ( Empty , Empty ) -> true
| ( Node( x, llh, lrh ), Node( y, rlh, rrh ) ) -> ( aux llh rrh ) && ( aux lrh rlh )
| _ -> false in
match root with
| Empty -> false
| Node( _, lh, rh ) -> aux lh rh;;

let construct list = 
let rec insert_tree h = function
| Empty -> Node( h, Empty, Empty )
| Node( x, lh, rh ) as n -> if x = h then n else ( if h < x then Node( x, insert_tree h lh, rh ) else Node( x, lh, insert_tree h rh ) ) in
let rec aux acc = function
| [] -> acc
| h::t -> aux ( insert_tree h acc ) t in
aux Empty list;;


is_symmetric(construct [5;3;18;1;4;12;21]);;
not(is_symmetric(construct [3;2;5;7;4]));;
