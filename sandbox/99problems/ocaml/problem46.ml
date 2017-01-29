type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let construct list = 
let rec insert_tree h = function
| Empty -> Node( h, Empty, Empty )
| Node( x, lh, rh ) as n -> if x = h then n else ( if h < x then Node( x, insert_tree h lh, rh ) else Node( x, lh, insert_tree h rh ) ) in
let rec aux acc = function
| [] -> acc
| h::t -> aux ( insert_tree h acc ) t in
aux Empty list;;

construct [3;2;5;7;1];;

