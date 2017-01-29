type 'a rle =
| One of 'a
| Many of int * 'a;;

let decode list = 
let rec expand acc n c = if n = 0 then acc else  expand ( c :: acc ) ( n - 1 ) c in
let rec aux acc = function
| [] -> acc
| One( x ) :: rest -> aux (x::acc) rest
| Many( n, x ) :: rest -> aux ( expand acc n x ) rest in
List.rev ( aux [] list );;

decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
