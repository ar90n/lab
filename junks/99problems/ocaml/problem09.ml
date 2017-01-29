let pack l =
let rec aux res acc = function
| [] -> acc :: res
| [a] -> ( a :: acc ) :: res
| a :: ( b :: _ as t ) -> if a = b then ( aux res ( a :: acc ) t )
else ( aux ( ( a :: acc ) :: res ) [] t ) in
List.rev ( aux [] [] l );;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
