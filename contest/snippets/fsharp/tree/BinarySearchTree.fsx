module BinarySearchTree =
    open BinaryTree
    type 'a binsearchtree = ('a -> 'a -> int) * 'a bintree

    let init (comp : 'a -> 'a -> int) : 'a binsearchtree =
        (comp, Empty ) 

    let rec contain (bstree : 'a binsearchtree) (v : 'a) : bool =
        let (comp, tree) = bstree
        let rec contain' (tree : 'a bintree) (v : 'a) : bool =
            match tree with
            | Empty -> false
            | Node( v', lh, rh) when comp v' v <  0 -> contain' rh v
            | Node( v', lh, rh) when comp v  v'<  0 -> contain' lh v
            | _ -> true
        contain' tree v

    let rec min (bstree : 'a binsearchtree) : 'a option =
        let (comp, tree) = bstree
        let rec min' (tree : 'a bintree) : 'a option =
            match tree with
            | Empty -> None
            | Node( v, Empty, _) -> Some v
            | Node( _, lh, _) -> min' lh
        min' tree

    let rec max (bstree : 'a binsearchtree) : 'a option =
        let (comp, tree) = bstree
        let rec max' (tree : 'a bintree) : 'a option =
            match tree with
            | Empty -> None
            | Node( v, _, Empty) -> Some v
            | Node( _, _, rh) -> max' rh
        max' tree

    let rec insert (bstree : 'a binsearchtree) (v : 'a) : 'a binsearchtree=
        let (comp, tree) = bstree
        let rec insert' (tree : 'a bintree) (v : 'a) : 'a bintree =
            match tree with
            | Empty -> Node( v, Empty, Empty)
            | Node( v', lh, rh) when comp v' v  < 0 -> Node( v', lh, insert' rh v)
            | Node( v', lh, rh) when comp v  v' < 0 -> Node( v', insert' lh v, rh)
            | n -> n
        insert' tree v |> (fun t -> (comp, t))

    let rec delete (bstree : 'a binsearchtree) (v : 'a) =
        let (comp, tree) = bstree
        let rec delete' (tree : 'a bintree) (v : 'a) =
            match tree with
            | Empty -> Empty
            | Node( v', lh, rh) when comp v' v  < 0 -> Node( v', lh, delete' rh v)
            | Node( v', lh, rh) when comp v  v' < 0 -> Node( v', delete' lh v, rh)
            | Node( v', lh, Empty) when comp v  v' = 0 -> lh
            | Node( v', Empty, rh) when comp v  v' = 0 -> rh
            | Node( v', Empty, Empty) when comp v  v' = 0 -> Empty
            | Node( v', lh, rh) when comp v  v' = 0 -> match min (comp, rh) with
                                                                  | Some nv -> Node( nv, lh, delete' rh nv )
                                                                  | None -> failwith "Error"
            | _ -> failwith "Error"
        delete' tree v

    let ofList (comp : 'a -> 'a -> int) (lst : 'a list) =
        lst
        |> List.fold (fun tree v -> insert tree v) (init comp)

    let toBinaryTree (bstree : 'a binsearchtree) : 'a bintree =
        snd
