open List;;

let rotate list n =
let len = List.length list in
let sn = ( n + len ) mod len in
let rec aux acc nn = function
| [] -> List.rev acc
| _ as t when nn = 0 -> t @ List.rev( acc )
| head::rest -> aux (head::acc) ( nn - 1 ) rest in
aux [] sn list;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 0;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
