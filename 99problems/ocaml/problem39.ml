let all_primes b e =
let rec aux acc n e =
if n <= e then ( if ( List.for_all ( fun x -> ( n mod x ) <> 0 ) acc ) then aux (n::acc) (n+1) e else aux acc (n+1) e ) else acc in
List.filter (fun x -> b <= x ) ( aux [2] 3 e );;

let goldbach n =
let primes = all_primes 2 n in
let prime_pairs = List.fold_left ( fun a x -> a @ ( List.map ( fun y -> ( x, y) ) primes ) ) [] primes in
List.hd ( List.rev ( List.filter ( fun (x,y) -> ( ( x + y ) = n ) ) prime_pairs ) );;

let goldbach_list b e =
let primes = all_primes 2 e in
let prime_pairs = List.fold_left ( fun a x -> a @ ( List.map ( fun y -> ( x, y ) ) primes ) ) [] primes in
List.map ( fun (x,y) -> ( x + y, ( x, y ) ) ) ( List.filter ( fun (x,y) -> ( x <= y ) && ( ( ( x + y ) mod 2 ) = 0 ) && ( ( x + y ) <= e ) ) prime_pairs );;

goldbach_list 9 20;;
