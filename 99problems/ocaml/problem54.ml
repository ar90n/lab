type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let take n l =
    let rec aux acc n l = match ( n, l ) with
        | ( 0, _ ) -> ( acc, l )
        | ( _, [] ) -> ( acc, [] )
        | ( n, h::t ) -> aux (h::acc)  (n-1) t in
    let t,r = aux [] n l in
    ( List.rev t, r ) in

let split_node lst =
    let take_rl n l = 
        let elms = ( 1 lsl n ) in
        let lh,r = take elms l in
        let rh,r = take elms r in
        ( lh, rh, r ) in
    let rec aux lh rh n l = match l with
        | [] -> ( lh, rh )
        | _ -> let ( clh, crh, cr ) = take_rl n l in
            aux ( lh @ clh ) ( rh @ crh ) ( n + 1 ) cr in
    aux [] [] 0 lst in

let rec complete_binary_tree = function
    | [] -> Empty
    | [h] -> Node( h, Empty, Empty )
    | h::t -> let lh,rh = split_node t in
              Node( h, complete_binary_tree lh, complete_binary_tree rh ) in

complete_binary_tree [1;2;3;4;5;6];;
