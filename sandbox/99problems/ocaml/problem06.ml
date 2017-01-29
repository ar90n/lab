open List;;
(*
let is_palindrome l =
let f = l and
b = List.rev l in
let rec str_cmp l1 l2 =
match ( l1, l2 ) with
| ( [], [] ) -> true
| ( _, [] ) -> true
| ( [], _ ) -> true
| ( fh::fr, bh::br ) -> if fh = bh then str_cmp fr br else false in
str_cmp f b;;
*)

let is_palindrome l = l = List.rev l;;


is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
not (is_palindrome [ "a" ; "b" ]);;
