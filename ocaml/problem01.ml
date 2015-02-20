let rec last = function
| [] -> None
| head :: [] -> Some head
| _ :: rest -> last rest;;

last [ "a" ; "b" ; "c" ; "d" ];;
last [];;
