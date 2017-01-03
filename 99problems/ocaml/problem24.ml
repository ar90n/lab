let lotto_select n e =
let rec aux acc = function
| 0 -> acc
| c -> aux ( ( Random.int e )::acc ) ( c - 1 ) in
aux [] n;;

lotto_select 6 49;;
