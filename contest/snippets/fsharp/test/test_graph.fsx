#!/usr/bin/env fsharpi

open System;

let ParseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let ParseInt1 ( str : string ) =
    ParseIntN str |> fun arr -> arr.[0]

let ParseInt2 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1])

let ParseInt3 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

//--------------------------------------------------------------------------------

type cost = float
type vertex = int
type edge = vertex * vertex * cost
type graph = System.Collections.Generic.Dictionary<vertex, cost> array

let DGraph (n : int) (edges : edge list) : graph =
    let g : graph = Array.init n (fun _ -> new System.Collections.Generic.Dictionary< vertex, cost>())
    List.iter (fun (f,t,c) -> g.[f].[t] <- c) edges
    g

let Graph (n : int) (edges : edge list) : graph =
    let redges = List.map (fun (f,t,c) -> (t,f,c)) edges
    DGraph n <| List.append edges redges

let Nodes (g: graph) : int =
    Array.length g

let Edges (g: graph) : edge list =
    seq { for i = 0 to ( Nodes g ) - 1 do yield (i,Seq.zip g.[i].Keys g.[i].Values) } 
    |> Seq.map (fun (f,ts) -> Seq.map (fun (t,c) -> ( f,t,c ) ) ts )
    |> Seq.fold Seq.append Seq.empty
    |> Seq.toList

let Transpose (g : graph) : graph =
    g
    |> Edges 
    |> List.map (fun (f,t,c) -> (t,f,c)) 
    |> DGraph (Nodes g) 

let Dump (g : graph) : unit =
    for i = 0 to (Nodes g) - 1 do
        let v = Seq.zip g.[i].Keys g.[i].Values
        printfn "%d :%A" i v
    done

let Path (g : graph) (src : vertex) (dst : vertex) : vertex list option =
    let nodes = Nodes g
    let visited = Array.create nodes -1
    let queue = new System.Collections.Generic.Queue< vertex * vertex >()
    queue.Enqueue (src, src)
    while( queue.Count <> 0 ) do
        let (f,t) = queue.Dequeue()
        if visited.[t] = -1 then
            visited.[t] <- f
            g.[t].Keys |> Seq.iter ( fun tt -> queue.Enqueue (t,tt) )
    done

    let mutable result = [dst]
    while ( List.head result <> src ) && ( List.head result <> -1 ) do
        let c = List.head result
        result <- visited.[ c ] :: result
    done
    if (List.head result <> -1) then Some result else None

let BellmanFord (g : graph) (root : vertex) : cost list option =
    let nodes = Nodes g
    let result = Array.init nodes ( fun _ -> System.Double.PositiveInfinity)

    let changed = ref false
    result.[root] <- 0.0
    for i = 0 to (nodes - 1) do
        changed := false
        for f = 0 to (nodes - 1) do
            Seq.zip g.[f].Keys g.[f].Values 
            |> Seq.iter (fun (t,c) -> let nc = result.[f] + c
                                      if nc < result.[t] then 
                                        result.[t] <- nc
                                        changed := true )
        done
    done

    if !changed then None else Some (Array.toList result)

let Bfs (g : graph) (root : vertex) : vertex list * cost array = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let mutable costs = Array.create nodes 0.0
    let queue = new System.Collections.Generic.Queue< vertex * cost >()
    queue.Enqueue (root, 0.0)
    while( queue.Count <> 0 ) do
        let c,cc = queue.Dequeue()
        if not visited.[c] then
            visited.[c] <- true
            result <- c :: result
            costs.[c] <- cc
            Seq.zip g.[c].Keys g.[c].Values
            |> Seq.map (fun (i,c) -> (i, cc + c))
            |> Seq.iter queue.Enqueue
    done

    List.rev result, costs

let Dfs (g : graph) (root : vertex) : vertex list * cost array = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let mutable costs = Array.create nodes 0.0
    let stack = new System.Collections.Generic.Stack< vertex * cost >()
    stack.Push (root, 0.0)
    while( stack.Count <> 0 ) do
        let c,cc = stack.Pop()
        if not visited.[c] then
            visited.[c] <- true
            result <- c :: result
            costs.[c] <- cc
            Seq.zip g.[c].Keys g.[c].Values
            |> Seq.map (fun (i,c) -> (i, cc + c))
            |> Seq.iter stack.Push
    done

    List.rev result, costs

let Diameter (g : graph) : cost =
    Bfs g 0
    |> snd
    |> Seq.mapi (fun i u -> i,u)
    |> Seq.maxBy snd
    |> fst
    |> Bfs g
    |> snd
    |> Array.max

module PriorityQueue =
    type 'a _heap =
        | Empty
        | Node of int * 'a * 'a _heap * 'a _heap

    let rec _merge (comp : 'a -> 'a -> int) (h1 : 'a _heap) (h2 : 'a _heap) =
        let Height = function
            | Empty -> 0
            | Node(h, _, _, _) -> h
        let MakeTree x a b =
            let ha = Height a 
            let hb = Height b
            if ha >= hb then Node(hb + 1, x, a, b) else Node(ha + 1, x, b, a)
        match h1,h2 with
        | x, Empty -> x
        | Empty, x -> x
        | (Node(_, x, l1, r1) as n1), (Node(_, y, l2, r2) as n2) when comp x y <= 0 -> MakeTree x l1 (_merge comp r1 n2)
        | (Node(_, x, l1, r1) as n1), (Node(_, y, l2, r2) as n2) when comp x y > 0 -> MakeTree y l2 (_merge comp n1 r2)
        | _,_ -> failwith "empty"

    let _head (h : 'a _heap) =
        match h with
        | Empty -> failwith "empty"
        | Node(h, x, l, r) -> x

    let _tail (comp : 'a -> 'a -> int) (h : 'a _heap) =
        match h with
        | Empty -> failwith "empty"
        | Node(_, _, l, r) -> _merge comp l r

    type 'a pqueue = ('a -> 'a -> int)  * 'a _heap
    let init (comp : 'a -> 'a -> int)  =
        ( comp , Empty )

    let isEmpty (q : 'a pqueue) : bool =
        q |> function | ( _ , h )  -> h = Empty

    let top (q : 'a pqueue) =
        q |> function | ( _ , h )  -> _head h

    let push (q : 'a pqueue) (v : 'a) =
        q |> function | ( c , h )  -> Node(1,v,Empty,Empty) |> _merge c h |> fun h' -> ( c, h' )

    let pop (q : 'a pqueue) =
        q |> function | ( c , h )  -> (_head h, ( c, _tail c h) )

    let rec toSeq (q : 'a pqueue) =
        q |> pop |> fun (h,t) -> seq{ yield h; yield! toSeq t }

let Dijkstra (g : graph) (root : vertex) : (graph * cost array) =
    let nodes = Nodes g
    let costs = Array.create nodes System.Double.PositiveInfinity

    let comp (_,a) (_,b) = 
        match a,b with
        | _ when a < b -> -1
        | _ when a > b ->  1
        | _ -> 0
    let pqueue = Edges g |> List.fold (fun q (f,t,c) -> PriorityQueue.push q ((f,t,c),c)) (PriorityQueue.init comp)
    costs.[root] <- 0.0
    let rec doit (costs : cost array) (edges : edge list) (pqueue : (edge * cost) PriorityQueue.pqueue) : cost array * edge list =
        if PriorityQueue.isEmpty pqueue then
            costs, edges
        else
            let (((_,t,c) as e,ac),pqueue') = PriorityQueue.pop pqueue
            if ac < costs.[t] then
                let edges' = e :: edges
                costs.[t] <- ac
                Seq.zip g.[t].Keys g.[t].Values
                |> Seq.map (fun (t',c') -> (t,t',c'))
                |> Seq.fold ( fun q (f,t,c) -> PriorityQueue.push q ((f,t,c), ac + c) ) pqueue'
                |> doit costs edges'
            else
                doit costs edges pqueue'
    let (rc,re ) = doit costs [] pqueue
    DGraph nodes re, rc

let Prim (g : graph) (root : vertex) : graph =
    let nodes = Nodes g
    let visited = Array.create nodes false
    let comp (_,_,a) (_,_,b) = 
        match a,b with
        | _ when a < b -> -1
        | _ when a > b ->  1
        | _ -> 0
    let pqueue = ref <| PriorityQueue.init comp
    Seq.zip g.[root].Keys g.[root].Values |> Seq.iter (fun (t,c) -> pqueue := PriorityQueue.push !pqueue (root,t,c) )

    visited.[root] <- true
    let mutable edges = []
    while( not (PriorityQueue.isEmpty !pqueue) ) do
        let (e,npqueue) = PriorityQueue.pop !pqueue
        let (f,t,c) = e
        pqueue := npqueue

        if not visited.[t] then
            visited.[t] <- true
            edges <- e :: edges
            Seq.zip g.[t].Keys g.[t].Values |> Seq.iter (fun (tt,c) -> pqueue := PriorityQueue.push !pqueue (t,tt,c) )
    done
    DGraph nodes edges

let TopoSort (g : graph) : vertex list option = 
    let nodes = Nodes g
    let status = Array.create nodes 0
    let result = ref []
    let rec visit (v : vertex) = 
        match status.[v] with
        | 0 -> status.[v] <- 1
               g.[v].Keys |> Seq.iter visit
               status.[v] <- 2
               result := v :: !result
        | 1 -> failwith "has cyclic"
        | _ -> ignore()

    try
        seq {0..(nodes-1)} |> Seq.iter (fun v -> if status.[v] = 0 then visit v)
        Some <| List.rev !result
    with
        | Failure "has cyclic" -> None

type UnionFind = int[] * int[]

let Root ( uf : UnionFind ) ( i : int ) =
    let (id,sz) = uf
    let mutable q = i
    while ( q <> id.[q] ) do
        id.[q] <- id.[id.[q]]
        q <- id.[q]
    q

let Find ( uf : UnionFind ) ( p : int ) ( q : int ) =
    (Root uf p) = (Root uf q)

let Unite ( uf : UnionFind ) ( p : int ) ( q : int ) =
    let (id,sz) = uf
    let i = Root uf p
    let j = Root uf q
    if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
    else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]

let WarshallFloyd (g : graph) : cost[,] =
    let nodes = Nodes g
    let costs = Array2D.init nodes nodes (fun f t -> if g.[f].ContainsKey t then g.[f].[t] else System.Double.PositiveInfinity)
    for m = 0 to (nodes - 1) do
        for f = 0 to (nodes - 1) do
            for t = 0 to (nodes - 1) do
                costs.[f,t] <- min  costs.[f, t] (costs.[f,m] + costs.[m,t])
            done
        done
    done
    costs

//--------------------------------------------------------------------------------
Console.WriteLine "OK"