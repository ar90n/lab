module BinarySearchTree =
    type 'a binsearchtree = ('a -> 'a -> int) * 'a BinaryTree.bintree

    let init (comp : 'a -> 'a -> int) : 'a binsearchtree =
        (comp, BinaryTree.Empty ) 

    let rec contain (bstree : 'a binsearchtree) (v : 'a) : bool =
        let (comp, tree) = bstree
        let rec contain' (tree : 'a BinaryTree.bintree) (v : 'a) : bool =
            match tree with
            | BinaryTree.Empty -> false
            | BinaryTree.Node( v', lh, rh) when comp v' v <  0 -> contain' rh v
            | BinaryTree.Node( v', lh, rh) when comp v  v'<  0 -> contain' lh v
            | _ -> true
        contain' tree v

    let rec min (bstree : 'a binsearchtree) : 'a option =
        let (comp, tree) = bstree
        let rec min' (tree : 'a BinaryTree.bintree) : 'a option =
            match tree with
            | BinaryTree.Empty -> None
            | BinaryTree.Node( v, BinaryTree.Empty, _) -> Some v
            | BinaryTree.Node( _, lh, _) -> min' lh
        min' tree

    let rec max (bstree : 'a binsearchtree) : 'a option =
        let (comp, tree) = bstree
        let rec max' (tree : 'a BinaryTree.bintree) : 'a option =
            match tree with
            | BinaryTree.Empty -> None
            | BinaryTree.Node( v, _, BinaryTree.Empty) -> Some v
            | BinaryTree.Node( _, _, rh) -> max' rh
        max' tree

    let rec insert (bstree : 'a binsearchtree) (v : 'a) : 'a binsearchtree=
        let (comp, tree) = bstree
        let rec insert' (tree : 'a BinaryTree.bintree) (v : 'a) : 'a BinaryTree.bintree =
            match tree with
            | BinaryTree.Empty -> BinaryTree.Node( v, BinaryTree.Empty, BinaryTree.Empty)
            | BinaryTree.Node( v', lh, rh) when comp v' v  < 0 -> BinaryTree.Node( v', lh, insert' rh v)
            | BinaryTree.Node( v', lh, rh) when comp v  v' < 0 -> BinaryTree.Node( v', insert' lh v, rh)
            | n -> n
        insert' tree v |> (fun t -> (comp, t))

    let rec delete (bstree : 'a binsearchtree) (v : 'a) =
        let (comp, tree) = bstree
        let rec delete' (tree : 'a BinaryTree.bintree) (v : 'a) =
            match tree with
            | BinaryTree.Empty -> BinaryTree.Empty
            | BinaryTree.Node( v', lh, rh) when comp v' v  < 0 -> BinaryTree.Node( v', lh, delete' rh v)
            | BinaryTree.Node( v', lh, rh) when comp v  v' < 0 -> BinaryTree.Node( v', delete' lh v, rh)
            | BinaryTree.Node( v', lh, BinaryTree.Empty) when comp v  v' = 0 -> lh
            | BinaryTree.Node( v', BinaryTree.Empty, rh) when comp v  v' = 0 -> rh
            | BinaryTree.Node( v', BinaryTree.Empty, BinaryTree.Empty) when comp v  v' = 0 -> BinaryTree.Empty
            | BinaryTree.Node( v', lh, rh) when comp v  v' = 0 -> match min (comp, rh) with
                                                                  | Some nv -> BinaryTree.Node( nv, lh, delete' rh nv )
                                                                  | None -> failwith "Error"
            | _ -> failwith "Error"
        delete' tree v

    let ofList (comp : 'a -> 'a -> int) (lst : 'a list) =
        lst
        |> List.fold (fun tree v -> insert tree v) (init comp)

    let toBinaryTree (bstree : 'a binsearchtree) : 'a bintree =
        snd
