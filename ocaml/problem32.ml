let gcd a b =
let ma = max a b in
let mi = min a b in
let rec aux a b = if b = 0 then a else aux b ( a mod b ) in
aux ma mi;;

let coprime a b = if ( gcd a b ) = 1 then true else false;;

let phi n = 
let rec aux c m =  if m = 0 then c else ( if ( coprime n m ) then aux ( c + 1 ) ( m - 1) else aux c ( m - 1 ) ) in
aux 0 n;;

phi 10;;
phi 13;;
