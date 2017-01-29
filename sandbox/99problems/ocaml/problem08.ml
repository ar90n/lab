(*
let compress list = 
let rec aux acc p = function
| [] -> acc
| head :: rest when acc = [] -> aux [head] head rest
| head :: rest -> if head = p then ( aux acc p rest )
                              else ( aux ( head :: acc ) head rest ) in
List.rev ( aux [] "x" list );;
*)

let rec compress = function
| a :: ( b :: _ as t ) -> if a = b then compress t else a :: compress t
| smaller -> smaller;;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
