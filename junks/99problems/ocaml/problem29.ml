let is_prime n =
  let rec aux m = match m with
  | x when ( x * x ) <= n -> if ( n mod x ) = 0 then false else aux ( m + 1 )
  | _ -> true in
  if n <= 1 then false else aux 2;;

not(is_prime 1);;
is_prime 7;;
not(is_prime 12);;
