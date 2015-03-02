let factors n =
let rec aux acc m n = if m * m < n then ( if ( n mod m ) = 0 then aux (m::acc) m ( n / m ) else aux acc ( m + 1 ) n ) else n::acc in
aux [] 2 n;;

factors 315;;
