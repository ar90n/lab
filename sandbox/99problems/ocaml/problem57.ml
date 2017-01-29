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

let layout_binary_tree_3 t = 
    let rec num_of_elems_to_boundary f n t = match t with
        | Empty -> n
        | Node( _, lh, rh ) as node ->  num_of_elems_to_boundary f ( n + 1 ) ( f node ) in
    let lr_boundary_elems t =  match t with
                                | Empty -> ( 0, 0 )
                                | Node( _, lh, rh ) -> ( ( num_of_elems_to_boundary ( fun n -> match n with
                                                                                                | Node( _, _, rh ) -> rh
                                                                                                | _ as n -> n )
                                                                                  0 lh ),
                                                         ( num_of_elems_to_boundary ( fun n -> match n with
                                                                                                | Node( _, lh, _ ) -> lh
                                                                                                | _ as n -> n )
                                                                                  0 rh ) ) in
    let root_branch_length t = let between_lh_elems, between_rh_elems = lr_boundary_elems t in
                               max ( min between_lh_elems  between_rh_elems ) 1 in
    let rec set_pos x y t = let branch_length = root_branch_length t in
                            match t with
                                | Empty -> E
                                | Node( w, lh, rh ) -> N( w, x, y, ( set_pos ( x - branch_length ) ( y + 1 ) lh ), ( set_pos ( x + branch_length ) ( y + 1) rh ) ) in
    let rec refine_x_pos offset t = match t with
                                    | E -> E
                                    | N( w, x, y, lh, rh ) -> N( w, x + offset, y, ( refine_x_pos offset lh ), ( refine_x_pos offset rh ) ) in
    let get_x_offset t = 
        let rec aux n t = match t with
                            | E -> n
                            | N( _, _, _, lh, _ ) -> aux ( n + 1 ) lh in
        aux 1 t in
    let tmp_tree = set_pos 0 1 t in
    refine_x_pos ( get_x_offset tmp_tree ) tmp_tree;;

layout_binary_tree_3 example_layout_tree;;
