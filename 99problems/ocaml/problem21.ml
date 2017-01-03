let insert_at ss nn list =
let rec aux acc s n = function
| [] -> acc
| head::rest when n = 1 -> aux (s::head::acc) s (-1) rest
| head::rest -> aux (head::acc) s ( n - 1 ) rest in
List.rev ( aux [] ss nn list );;

insert_at "alfa" 1 ["a";"b";"c";"d"];;
insert_at "alfa" 3 ["a";"b";"c";"d"];;
insert_at "alfa" 4 ["a";"b";"c";"d"];;
