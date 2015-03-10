type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec hbal_tree n =
let merge_tree acc lh rh = List.fold_left ( fun acc x -> List.fold_left ( fun acc y -> Node( 'x', x, y )::acc ) acc rh ) acc lh in
match n with
| n when n <= 0 -> [Empty]
| n when n = 1 -> let t1 = hbal_tree ( n - 1 ) in merge_tree [] t1 t1
| n -> let t1 = hbal_tree ( n - 1 ) and t2 = hbal_tree ( n - 2 ) in merge_tree ( merge_tree ( merge_tree [] t2 t1 ) t1 t2 ) t1 t1;;

let t = hbal_tree 3;;
let x = 'x';;
List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)))) t;;
List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
Node(x, Node(x, Empty, Empty), Empty))) t;;
List.length t;;
