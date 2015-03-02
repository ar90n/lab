let timeit f a =
let t0 = Sys.time() in
ignore( f a );
let t1 = Sys.time() in
t1 -. t0;;

let gcd a b =
let ma = max a b in
let mi = min a b in
let rec aux a b = if b = 0 then a else aux b ( a mod b ) in
aux ma mi;;

let coprime a b = if ( gcd a b ) = 1 then true else false;;

let phi n = 
let rec aux c m =  if m = 0 then c else ( if ( coprime n m ) then aux ( c + 1 ) ( m - 1) else aux c ( m - 1 ) ) in
aux 0 n;;

let factors nn =
let rec aux acc m n =
let v,c = m in
if v <= nn then ( if ( n mod v ) = 0 then aux acc (v,c+1) ( n / v ) else aux (m::acc) ( v+1, 0 ) n ) else acc in
List.filter (fun x -> 0 < snd x ) ( aux [] (2,0) nn );;

let phi_improved n =
let rec pow n m =  if m > 0 then n * ( pow n ( m - 1 ) ) else 1 in
let num,den = List.split ( factors n ) in
let phi_num = List.fold_left ( fun x y -> x * ( y - 1 ) ) 1 num in
let phi_den = List.fold_left2 ( fun x y z -> x * (pow y ( z - 1 ) ) )  1 den num in
phi_num * phi_den;;

timeit phi 10090;;
timeit phi_improved 10090;;
