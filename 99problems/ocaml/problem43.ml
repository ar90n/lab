let huffman fs =
let codes = List.map ( fun (c,f) -> ([(c,"")],f) ) in
let freq_sort = List.sort (fun x y -> match ( snd x, snd y ) with 
| x,y when x < y -> -1
| x,y when x = y -> 0
| x,y when y < x -> 1 ) in
let add_one_bit b (cs,f) = ( List.map (fun (a,v) -> ( a, b ^ v ) ) cs, f ) in
let combine_code (acs,af) (bcs,bf) = ( (acs @ bcs) , af + bf ) in
let rec gen_code = function
| [] as l -> l
| [x] as l -> l
| a::(b::t) -> gen_code ( freq_sort ( ( combine_code ( add_one_bit "0" a ) ( add_one_bit "1" b ) )::t ) ) in
fst ( List.nth ( gen_code ( freq_sort (codes fs) ) ) 0 );;

let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ];;
huffman fs;;
