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

type cost = int64
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
    |> Seq.collect (fun (f,ts) -> Seq.map (fun (t,c) -> ( f,t,c ) ) ts )
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

module UnionFind =
    type _unionfind = int[] * int[]

    let create (n : int) : _unionfind =
        let id = Array.init n (fun i -> i)
        let sz = Array.create n 1
        (id,sz)

    let count (uf : _unionfind) ( i : int ) =
        uf |> function | (_,sz) -> sz.[i]

    let rec root ( uf : _unionfind ) ( i : int ) =
        let (id,sz) = uf
        if i <> id.[i] then id.[i] <- root uf id.[i]
        id.[i]

    let connected ( uf : _unionfind ) ( p : int ) ( q : int ) =
        (root uf p) = (root uf q)

    let unite ( uf : _unionfind ) ( p : int ) ( q : int ) =
        let i = root uf p
        let j = root uf q
        let (id,sz) = uf
        if i <> j then
            if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
            else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]


let Kruskal (g : graph) (root : vertex) : graph =
    let uf = Nodes g |> UnionFind.create
    Edges g
    |> List.sortBy (fun (_,_,c) -> c)
    |> List.fold (fun acc (a,b,c) -> if UnionFind.connected uf a b then acc else UnionFind.unite uf a b; (a,b,c)::acc) []
    |> DGraph (Nodes g)

let n,m = Console.ReadLine() |> ParseInt2
let es = ReadLinesN m
         |> Seq.toList
         |> List.map ParseInt3
         |> List.map (fun (a,b,c) -> (a-1,b-1,(int64 c)))
let g = Graph n es
let cs = Kruskal g 0
        |> Edges
        |> List.map (fun (_,_,c) -> c)

let mc = List.max cs
let sc = List.sum cs
let e0,e1 = List.partition (fun (_,_,c) -> c < mc) es
let uf = UnionFind.create n
e0
|> List.iter (fun (a,b,_) -> UnionFind.unite uf a b)
let s = e1
        |> List.filter (fun (a,b,_) -> not (UnionFind.connected uf a b))
        |> List.length
let r = sc - mc
printfn "%d %d" r s
