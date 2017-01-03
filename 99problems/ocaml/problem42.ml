let rec gray n =
if n = 1 then ["0";"1"] else ( let c = gray ( n - 1) in ( List.map ( fun x -> "0" ^ x ) c ) @ (List.map ( fun x -> "1" ^ x ) ( List.rev c ) ) );;

gray 1;;
gray 2;;
gray 3;;
