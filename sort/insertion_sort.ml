open Printf

let nextrand seed =
    let a = 16807.0 and m = 2147483647.0 in
    let t = a *. seed in
        t -. m *. floor ( t /. m );;

let rec randlist n seed tail =
    if n = 0 then ( seed, tail )
    else randlist (n - 1) ( nextrand seed ) ( seed :: tail );;

let rec insertion x = function
    [] -> [x]
  | h::rest when x < h -> x :: h :: rest
  | h::rest -> h :: insertion x rest;;

let rec insertion_sort = function
    [] -> []
  | h::rest -> insertion h ( insertion_sort rest );;

let () = List.iter( printf "%f " ) ( insertion_sort ( snd ( randlist 10 1.0 [] ) ) );;
