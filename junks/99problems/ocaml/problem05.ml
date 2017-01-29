let rev l =
let rec rev_impl r ll = match ll with
| [] -> r
| h :: rest -> rev_impl ( h :: r ) rest in
rev_impl [] l;;

rev ["a" ; "b" ; "c"];;
