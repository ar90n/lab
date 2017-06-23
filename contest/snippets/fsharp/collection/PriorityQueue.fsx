module PriorityQueue =
    type 'a _heap =
        | Empty
        | Node of int * int * 'a * 'a _heap * 'a _heap

    let _makeHeap (x : 'a) (l : 'a _heap) (r : 'a _heap ) =
        (l,r) |> function | (Node(ls, lh, _ , _, _ ), Node(rs, rh, _, _, _ )) -> Node(ls + rs + 1, (max lh rh) + 1, x, l, r )
                          | (Node(ls, lh, _ , _, _ ), Empty) -> Node(ls + 1, lh + 1, x, l, r)
                          | (Empty, Node(rs, rh, _ ,_, _)) -> Node(rs + 1, rh + 1, x, l,r ) 
                          | (Empty, Empty) -> Node(1, 1, x, l,r ) 

    let _bubbleUp (comp : 'a -> 'a -> int)  (x : 'a) (l : 'a _heap) (r : 'a _heap) =
        (l,r) |> function | (Node(_, _, y, ll, lr), _) when ((comp y x) < 0) -> _makeHeap y (_makeHeap x ll lr) r
                          | (_, Node(_, _, z, rl, rr)) when ((comp z x) < 0) -> _makeHeap z l (_makeHeap x rl rr)
                          | (_, _) -> _makeHeap x l r

    let rec _bubbleDown (comp : 'a -> 'a -> int)  (x : 'a) (l : 'a _heap) (r : 'a _heap) =
        (l,r) |> function | (Node(_, _, y, ll, lr), Node(_, _, z, rl, rr)) when (((comp z x) < 0) && ((comp z y) < 0)) -> _makeHeap z  l (_bubbleDown comp x rl rr)
                          | (Node(_, _, y, ll, lr), Node(_, _, z, rl, rr)) when ((comp y x) < 0) -> _makeHeap y  (_bubbleDown comp x ll lr) r
                          | _ -> _makeHeap x l r

    let _floatRight (x : 'a) (l : 'a _heap) (r : 'a _heap) =
        r |> function | Empty -> _makeHeap x l r
                      | Node( _, _, y, rl, rr) -> _makeHeap y l (_makeHeap x rl rr)

    let _floatLeft (x : 'a) (l : 'a _heap) (r : 'a _heap) =
        l |> function | Empty -> _makeHeap x l r
                      | Node( _, _, y, ll, lr) -> _makeHeap y (_makeHeap x ll lr) r

    let rec _merge (l : 'a _heap) (r : 'a _heap) =
        (l,r) |> function | (Empty, Empty) -> Empty
                          | ( _, Empty) -> l
                          | ( Empty, _) -> r
                          | (Node(ls,lh, x, ll, lr) , _) when (ls < ((pown 2 lh) - 1)) -> _floatLeft x (_merge ll lr) r
                          | (_, Node(rs,rh, x, rl, rr)) when (rs < ((pown 2 rh) - 1)) -> _floatRight x l (_merge rl rr)
                          | (Node(_,lh, x, ll, lr), Node(_, rh, _, _, _)) when (rh < lh) -> _floatLeft x (_merge ll lr) r
                          | (_, Node(rs,rh, x, rl, rr)) -> _floatRight x l (_merge rl rr)

    let rec _insert (comp : 'a -> 'a -> int) (h : 'a _heap) (v : 'a) =
        h |> function | Empty -> _makeHeap v Empty Empty
                      | Node(s, h, x, l, r) -> (l,r) |> function | (Empty, _) -> _bubbleUp comp x (_insert comp l v) r
                                                                 | (Node(ls,lh, _, _, _), _) when (ls < ((pown 2 lh) - 1)) -> _bubbleUp comp x (_insert comp l v) r
                                                                 | (_, Empty) -> _bubbleUp comp x l (_insert comp r v)
                                                                 | (_, Node(rs,rh, _, _, _)) when (rs < ((pown 2 rh) - 1)) -> _bubbleUp comp x l (_insert comp r v)
                                                                 | (Node(_,lh,_, _, _), Node(_, rh, _, _, _)) when (rh < lh) -> _bubbleUp comp x l (_insert comp r v)
                                                                 | (_, _) -> _bubbleUp comp x (_insert comp l v) r

    let _head (h : 'a _heap) =
        h |> function | Empty -> failwith "empty"
                      | Node( _ , _, x, _, _) -> x

    let _tail (comp : 'a -> 'a -> int) (h : 'a _heap) =
        h |> function | Empty -> failwith "empty"
                      | Node( _, _, _, l, r ) -> _merge l r |> function | Empty -> Empty
                                                                        | Node( _, _, x, l2, r2 ) -> _bubbleDown comp x l2 r2

    type 'a pqueue = ('a -> 'a -> int)  * 'a _heap
    let init (comp : 'a -> 'a -> int)  =
        ( comp , Empty )

    let isEmpty (q : 'a pqueue) : bool =
        q |> function | ( _ , h )  -> h = Empty

    let top (q : 'a pqueue) =
        q |> function | ( _ , h )  -> _head h

    let push (q : 'a pqueue) (v : 'a) =
        q |> function | ( c , h )  -> v |> _insert c h |> fun h' -> ( c, h' )

    let pop (q : 'a pqueue) =
        q |> function | ( c , h )  -> (_head h, ( c, _tail c h) )

    let rec toSeq (q : 'a pqueue) =
        q |> pop |> fun (h,t) -> seq{ yield h; yield! toSeq t }
