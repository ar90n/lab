let split list n =
let rec aux less bigger n = function
| [] -> ( List.rev less, List.rev bigger )
| head::rest -> if 0 < n then aux ( head::less ) ( bigger ) ( n - 1 ) rest
else aux ( less ) ( head::bigger ) ( n - 1 ) rest in
aux [] [] n list;;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
