type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode list =
    let rec conv = function
        | [] as l -> l
        | ( n , c ) :: rest -> if n = 1 then One( c ) :: ( conv rest )
                                        else Many( n,c ) :: ( conv rest ) in
    let rec aux res n = function
        | [] as l -> l
        | [x] -> ( n + 1, x ) :: res
        | a :: ( b :: _ as t ) -> if a = b then aux res ( n + 1 ) t
        else aux (( n + 1, a )::res) 0 t in
    conv ( aux [] 0 list );;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
