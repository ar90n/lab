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
let PriorityQueue (comp : 'a -> 'a -> int)  =
    ( comp , Empty )

let IsEmpty (q : 'a pqueue) : bool =
    q |> function | ( _ , h )  -> h = Empty

let Top (q : 'a pqueue) =
    q |> function | ( _ , h )  -> _head h

let Push (q : 'a pqueue) (v : 'a) =
    q |> function | ( c , h )  -> Node(1,v,Empty,Empty) |> _merge c h |> fun h' -> ( c, h' )

let Pop (q : 'a pqueue) =
    q |> function | ( c , h )  -> (_head h, ( c, _tail c h) )

let rec toSeq (q : 'a pqueue) =
    q |> Pop |> fun (h,t) -> seq{ yield h; yield! toSeq t }

let cnt pq =
    let rec doit pq h c =
        if IsEmpty pq then
            c
        else
            let (h', pq') = Pop pq
            if h' = h then
                doit pq' h (c+1)
            else
                c
    doit pq (Top pq) 0

let (n,t) = ParseInt2 <| Console.ReadLine()
Console.ReadLine()
|> ParseIntN
|> Seq.fold (fun (pq,mv) a -> (Push pq (a - mv), min mv a)) (PriorityQueue (fun a b -> -compare a b), System.Int32.MaxValue)
|> fun (pq,_) -> cnt pq
|> Console.WriteLine

