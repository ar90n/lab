let factors nn =
let rec aux acc m n =
let v,c = m in
if v * v <= nn then ( if ( n mod v ) = 0 then aux acc (v,c+1) ( n / v ) else aux (m::acc) ( v+1, 0 ) n ) else acc in
List.filter (fun x -> 0 < snd x ) ( aux [] (2,0) nn );;

factors 315;;
