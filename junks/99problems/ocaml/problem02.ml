let rec last_two = function
| [] -> None
| [_] -> None
| [h1;h2] -> Some ( h1 , h2 )
| _ :: rest -> last_two rest;;

last_two [ "a" ; "b" ; "c" ; "d" ];;
last_two [ "a" ];;
