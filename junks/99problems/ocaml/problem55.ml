type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;

let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                              Node('h', Node('g', leaf 'e',Empty), Empty)),
                    leaf 'm'),
          Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;

let layout_binary_tree_1 t = 
    let rec dfs n ub t = match t with
        | Empty -> ( ub, E )
        | Node( w, lh, rh ) -> let lub , lt = dfs ( n + 1 ) ub lh in
                               let rub , rt = dfs ( n + 1 ) ( lub + 1 ) rh in
                               ( rub ,  N ( w, lub + 1, n, lt, rt ) ) in
    dfs 1 0 t in

layout_binary_tree_1 example_layout_tree;;
