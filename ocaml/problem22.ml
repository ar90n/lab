let range b e =
let step = if b < e then 1 else -1 in
let rec aux acc b e = if b != e then aux (b::acc) (b+step) e else b::acc in
List.rev ( aux [] b e );;

range 4 9;;
range 9 4;;
