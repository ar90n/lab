module BinaryTree =
    type 'a bintree =
        | Empty
        | Node of 'a * 'a bintree * 'a bintree

    let init =
        Empty

    let rec contain (tree : 'a bintree) (v : 'a) =
        match tree with
        | Empty -> false
        | Node( v', lh, rh) when v' < v -> contain rh v 
        | Node( v', lh, rh) when v < v' -> contain lh v
        | _ -> true

    let rec min (tree : 'a bintree) : 'a option =
        match tree with
        | Empty -> None
        | Node( v, Empty, _) -> Some v
        | Node( _, lh, _) -> min lh

    let rec max (tree : 'a bintree) : 'a option =
        match tree with
        | Empty -> None
        | Node( v, _, Empty) -> Some v
        | Node( _, _, rh) -> min rh

    let rec insert (tree : 'a bintree) (v : 'a) =
        match tree with
        | Empty -> Node( v, Empty, Empty)
        | Node( v', lh, rh) when v' < v -> Node( v', lh, insert rh v)
        | Node( v', lh, rh) when v < v' -> Node( v', insert lh v, rh)
        | n -> n

    let rec delete (tree : 'a bintree) (v : 'a) =
        match tree with
        | Empty -> Empty
        | Node( v', lh, rh) when v' < v -> Node( v', lh, delete rh v)
        | Node( v', lh, rh) when v < v' -> Node( v', delete lh v, rh)
        | Node( v', lh, Empty) when v = v' -> lh 
        | Node( v', Empty, rh) when v = v' -> rh 
        | Node( v', Empty, Empty) when v = v' -> Empty
        | Node( v', lh, rh) when v = v' -> match min rh with
                                           | Some nv -> Node( nv, lh, delete rh nv )
                                           | None -> failwith "Error"
        | _ -> failwith "Error"

    let ofList (lst : 'a list) =
        lst
        |> List.fold (fun tree v -> insert tree v) Empty
