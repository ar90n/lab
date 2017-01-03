let encode =
let rec aux res n = function
| [] as l -> l
| [x] -> ( n + 1, x ) :: res
| a :: ( b :: _ as t ) -> if a = b then aux res ( n + 1 ) t
else aux (( n + 1, a )::res) 0 t in
aux [] 0;;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
