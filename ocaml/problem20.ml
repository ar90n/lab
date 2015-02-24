let remove_at nn list =
let rec aux acc n = function
| [] -> acc
| head::rest when n = 0 -> aux acc (-1) rest
| head::rest -> aux ( head::acc ) ( n - 1 ) rest in
List.rev ( aux [] nn list );;

remove_at 1 ["a";"b";"c";"d"];;
