type 'a binary_tree =
| Empty
| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let internals list =
let rec aux acc = function
| Empty -> acc
| Node( _, Empty, Empty ) -> acc
| Node( x, lh, rh ) -> aux ( aux (x::acc) lh ) rh in
aux [] list;;

internals Empty;;
internals example_tree;;
