let gcd a b =
let ma = max a b in
let mi = min a b in
let rec aux a b = if b = 0 then a else aux b ( a mod b ) in
aux ma mi;;

let coprime a b = if ( gcd a b ) = 1 then true else false;;

coprime 13 27;;
not ( coprime 20536 7826 );;
