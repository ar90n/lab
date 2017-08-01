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
type graph = System.Collections.Generic.Dictionary<vertex, cost list> array

let DGraph (n : int) (edges : edge list) : graph =
    let g : graph = Array.init n (fun _ -> new System.Collections.Generic.Dictionary< vertex, cost list>())
    List.iter (fun (f,t,c) -> if not (g.[f].ContainsKey t) then g.[f].[t] <- []
                              g.[f].[t] <- c :: g.[f].[t]) edges
    g

let Graph (n : int) (edges : edge list) : graph =
    let redges = List.map (fun (f,t,c) -> (t,f,c)) edges
    DGraph n <| List.append edges redges

let Nodes (g: graph) : int =
    Array.length g

let Edges (g: graph) : edge list =
    seq { for i = 0 to ( Nodes g ) - 1 do yield (i,Seq.zip g.[i].Keys g.[i].Values) } 
    |> Seq.map (fun (f,ts) -> Seq.collect (fun (t,cs) -> List.map (fun c -> ( f,t,c )) cs ) ts )
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

let Bfs (g : graph) (root : vertex) = 
    let nodes = Nodes g
    let visited = Array2D.create nodes 1024 false
    let mutable costs = Array.create nodes System.Int32.MaxValue
    let queue = new System.Collections.Generic.Queue< vertex * cost >()
    queue.Enqueue (root, 0)
    while( queue.Count <> 0 ) do
        let c,cc = queue.Dequeue()
        if not visited.[c,cc] then
            visited.[c,cc] <- true
            costs.[c] <- min costs.[c] cc
            Seq.zip g.[c].Keys g.[c].Values
            |> Seq.toList
            |> List.iter (fun (i,cs) -> List.iter (fun x -> let cc' = cc ||| x
                                                            if not visited.[i,cc'] then queue.Enqueue (i, cc')) cs)
    done
    costs

let n,m = Console.ReadLine() |> ParseInt2
let g = ReadLinesN m
        |> Seq.toList
        |> List.map ParseInt3
        |> List.map (fun (a,b,c) -> (a-1, b-1, c))
        |> Graph n
let a,b = Console.ReadLine() |> ParseInt2 |> (fun (a,b) -> (a-1,b-1))
Bfs g a
|> (fun cs -> cs.[b])
|> function | System.Int32.MaxValue -> -1
            | x -> x
|> printf "%A"
