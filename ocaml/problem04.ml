let length l =
let rec length_impl n l = match l with
| [] -> n
| _ :: rest -> length_impl ( n + 1 ) rest in
length_impl 0 l;;

length [ "a" ; "b" ; "c"];;
length [];;
