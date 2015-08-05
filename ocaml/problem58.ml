type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_layout_tree =
         let leaf x = Node (x, Empty, Empty) in
             Node('a', Node('b', leaf 'd', leaf 'e'),
                 Node('c', Empty, Node('f', leaf 'g', Empty)));;

let rec string_of_tree t = match t with
    | Empty -> ""
    | Node( w, Empty, Empty ) -> ( String.make 1 w )
    | Node( w, lh, rh ) -> ( String.make 1 w ) ^ "(" ^ ( string_of_tree lh ) ^ "," ^ ( string_of_tree rh ) ^ ")";;

let tree_of_string s = 
    let trim_space s =
        let rec aux acc i = 
            if i < 0 then acc 
                     else aux ( if s.[i] <> ' ' then ( s.[i] :: acc ) else acc ) ( i - 1 ) in
        aux [] ( String.length s - 1 ) in
    let get_value lst = 
        let rec aux lst =
            match lst with
                | [] -> None
                | [h] -> Some( h, [] )
                | h::m::t when m = '(' || m = ')' || m = ',' -> Some ( h, t )
                | h::t -> aux t in
        aux lst in
    let get_child lst =
        let rec aux n acc lst = 
            match lst with
                | [] -> if n = 0 then Some ( List.rev acc, [] ) else None
                | h::t when h = ',' -> if n = 0 then Some ( List.rev acc, t ) else aux n ( h::acc ) t
                | h::t when h = '(' -> aux ( n + 1 ) ( h::acc ) t
                | h::t when h = ')' -> if n = 0 then Some ( List.rev acc, t ) else aux ( n - 1 ) ( h::acc ) t
                | h::t -> aux n ( h::acc ) t in
        aux 0 [] lst in
    let split_str lst = 
        match get_value lst with
            | None -> None
            | Some ( v, rem ) -> ( match get_child rem with
                                    | None -> None
                                    | Some( fst, rem ) -> ( match get_child rem with
                                                                | None -> None
                                                                | Some( snd ,rem ) -> Some ( v, fst, snd, rem ) ) ) in
    let rec aux lst = 
        if lst = [] then Empty 
                  else ( match split_str lst with
                            | None -> Empty
                            | Some ( v, fst, snd, rem ) ->Node( v, aux fst, aux snd ) ) in
    aux ( trim_space s );;

string_of_tree example_layout_tree;;
tree_of_string ( string_of_tree example_layout_tree );;
