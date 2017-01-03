type 'a node =
| One of 'a
| Many of 'a node list;;

let rec flatten = function
| [] -> []
| One( head ) :: rest -> head :: ( flatten rest )
| Many( head ) :: rest -> flatten( head ) @ flatten( rest );;

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
