let rec at n = function
| [] -> None
| head ::rest when n == 1 -> Some head
| _ :: rest when n > 1 -> at ( n - 1 ) rest;;

at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
at 3 [ "a" ];;
