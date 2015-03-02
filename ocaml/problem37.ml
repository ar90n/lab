let all_primes b e =
let rec aux acc n e =
if n <= e then ( if ( List.for_all ( fun x -> ( n mod x ) <> 0 ) acc ) then aux (n::acc) (n+1) e else aux acc (n+1) e ) else acc in
List.filter (fun x -> b <= x ) ( aux [2] 3 e );;

List.length( all_primes 2 7920);;
