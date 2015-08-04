type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
  Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
       Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let at_level list n =
  let rec aux acc n = function
    | Empty -> acc
    | Node( x, _, _ ) when n = 1 -> x::acc
    | Node( _, lh, rh ) -> aux ( aux acc ( n - 1 ) lh ) ( n - 1 ) rh in
  aux [] n list;;

at_level example_tree 2;;
at_level example_tree 5;;
