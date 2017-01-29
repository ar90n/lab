let drop list n =
let rec aux acc n = function
| [] -> acc
| head::rest when n = 0 -> aux ( head::acc ) 0 rest
| head::rest when n = 1 -> aux acc 0 rest
| head::rest -> aux ( head::acc ) ( n - 1 ) rest in
List.rev ( aux [] n list );;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
