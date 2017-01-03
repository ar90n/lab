open Printf

let rec partition pivot left right = function
    [] -> ( left, right )
  | h::rest when h < pivot -> partition pivot ( h::left ) right rest
  | h::rest -> partition pivot left ( h::right ) rest;;

let rec quick_sort = function
    [] -> []
  | h::rest -> let ( left, right ) = partition h [] [] rest in
    ( quick_sort left ) @ [h] @ ( quick_sort right );; 

let nextrand seed =
    let a = 16807.0 and m = 2147483647.0 in
    let t = a *. seed in
        t -. m *. floor ( t /. m );;

let rec randlist n seed tail =
    if n = 0 then ( seed, tail )
    else randlist (n - 1) ( nextrand seed ) ( seed :: tail );;

let () = List.iter( printf "%f " ) ( quick_sort ( snd ( randlist 10 1.0 [] ) ) );;
