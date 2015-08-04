type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;

let example_layout_tree =
    let leaf x = Node (x,Empty,Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                              Node('e', leaf 'd', leaf 'g')),
                    leaf 'm'),
         Node('u', Node('p', Empty, leaf 'q'), Empty));;

let layout_binary_tree_2 t = 
    let rec depth t = match t with
        | Empty -> 0
        | Node( _, lh, rh ) -> max ( ( depth lh ) + 1 ) ( ( depth rh ) + 1 ) in
    let rec origin_depth t = match t with
        | Empty -> 0
        | Node( _, lh, _ ) ->  1 + ( origin_depth lh ) in
    let d = depth t in
    let od = origin_depth t in
    let root_x = ( 1 lsl ( d - 1 ) ) - ( 1 lsl ( d - od - 1 ) ) in
    let rec wfs x y t = match t with
        | Empty -> E
        | Node( w, lh, rh ) -> let branch_step = 1 lsl ( d - y -1 ) in
                               let lx = x - branch_step in
                               let rx = x + branch_step in
                               N ( w, x, y, ( wfs lx ( y + 1 ) lh ), ( wfs rx ( y + 1 ) rh ) ) in
    wfs root_x 1 t;;

layout_binary_tree_2 example_layout_tree;;
