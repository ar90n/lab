type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let rec count_leaves = function
| Empty -> 0
| Node( _, Empty, Empty ) -> 1
| Node( _, lh, rh ) -> ( count_leaves lh ) + ( count_leaves rh );;

count_leaves Empty;;
count_leaves example_tree;;
