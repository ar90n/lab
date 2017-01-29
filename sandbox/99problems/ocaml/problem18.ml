let slice list b e =
let rec aux acc n = function
| [] -> List.rev acc
| head::rest when e < n -> List.rev acc
| head::rest when b <= n -> aux (head::acc) (n+1) rest
| _::rest -> aux acc (n+1) rest in
aux [] 0 list;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
