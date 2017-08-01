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

let ReadLinesN ( n : int ) =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.take n

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

type cost = int
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

let Dijkstra (g : graph) (fs : int[]) (root : vertex) : cost[,] =
    let nodes = Nodes g
    let costs = Array2D.create nodes 1024 System.Int32.MaxValue

    let comp (_,_,a) (_,_,b) = 
        match a,b with
        | _ when a < b -> -1
        | _ when a > b ->  1
        | _ -> 0
    let pqueue = Seq.zip g.[root].Keys g.[root].Values
                 |> Seq.map (fun (t',c') -> (root,t', c',  fs.[root] ||| fs.[t'] ))
                 |> Seq.fold (fun q (f,t,c,b) -> PriorityQueue.push q ((f,t,c),b,c)) (PriorityQueue.init comp)
    costs.[root,fs.[root]] <- 0
    let rec doit (costs : cost[,]) (pqueue : (edge * int * cost) PriorityQueue.pqueue) : cost[,]  =
        if PriorityQueue.isEmpty pqueue then
            costs
        else
            let (((_,t,c) as e, b, ac),pqueue') = PriorityQueue.pop pqueue
            let q'' = if ac < costs.[t,b] then
                        costs.[t,b] <- ac
                        Seq.zip g.[t].Keys g.[t].Values
                        |> Seq.filter (fun (t',c') -> (ac + c') < costs.[t',b ||| fs.[t']] )
                        |> Seq.fold (fun q (t',c') -> PriorityQueue.push q ((t,t',c'),b ||| fs.[t'], ac + c') ) pqueue'
                      else
                        pqueue'
            doit costs q''
    doit costs pqueue

let n,m,k = Console.ReadLine() |> ParseInt3
let fs = ReadLinesN n
         |> Seq.toArray
         |> Array.map (ParseIntN >> Seq.skip 1 >> Seq.fold (fun acc x -> acc ||| (1 <<< (x-1))) 0)
let g = ReadLinesN m
        |> Seq.toList
        |> List.map ParseInt3
        |> List.map (fun (a,b,c) -> (a-1,b-1,c))
        |> Graph n
let cs = Dijkstra g fs 0
let xs = List.filter (fun x -> cs.[n-1,x] <> System.Int32.MaxValue) [0..1023]
List.collect (fun x -> List.map (fun y -> (x|||y,max cs.[n-1,x] cs.[n-1,y])) xs) xs
|> List.filter (fun (a,_) -> a = ((1 <<< k)-1))
|> List.minBy (fun (_,a) -> a)
|> snd
|> printfn "%d"
