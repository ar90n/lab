
type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let is_symmetric root =
let rec aux lh rh = match ( lh, rh ) with
| ( Empty , Empty ) -> true
| ( Node( x, llh, lrh ), Node( y, rlh, rrh ) ) -> ( aux llh rrh ) && ( aux lrh rlh )
| _ -> false in
match root with
| Empty -> false
| Node( _, lh, rh ) -> aux lh rh;;

let rec cbal_tree = function
| 0 ->[Empty]
| n ->
let n = n - 1 in
let n1 = n / 2 in
let n2 = n - n1 in
let merge_tree l r all = List.fold_left ( fun acc x -> ( List.fold_left ( fun a y -> Node('x', x, y)::a ) acc r ) ) all l in
if n1 = n2 then (let h = cbal_tree n1 in merge_tree h h []) else( let lh = cbal_tree n1 in let rh = cbal_tree n2 in  merge_tree lh rh ( merge_tree rh lh [] ));;

let sym_cbal_trees n = List.filter is_symmetric ( cbal_tree n );;

List.length (sym_cbal_trees 57);;
