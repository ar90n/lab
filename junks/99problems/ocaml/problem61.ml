type 'a mult_tree = T of 'a * 'a mult_tree list;;

let count_nodes t = 
    let rec aux t = match t with
        | T( _, [] ) -> 1
        | T( _, l ) -> 1 + List.fold_left ( fun a b -> a + ( aux b ) ) 0 l in
    aux t;;

count_nodes (T('a', [T('f',[]) ]));;
