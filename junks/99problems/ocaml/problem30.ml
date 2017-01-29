let gcd a b =
let ma = max a b in
let mi = min a b in
let rec aux a b = if b = 0 then a else aux b ( a mod b ) in
aux ma mi;;

gcd 13 27;;
gcd 20536 7826;;
