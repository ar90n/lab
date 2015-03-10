type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let rec leaves list =
let rec aux acc = function
| Empty -> acc
| Node( x, Empty, Empty ) as n -> x::acc
| Node( _, lh, rh ) -> aux ( aux acc lh ) rh in
aux [] list;;

leaves Empty;;
leaves example_tree;;
