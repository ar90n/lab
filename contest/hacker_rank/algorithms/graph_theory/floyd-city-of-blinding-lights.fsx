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

let WarshallFloyd (g : graph) : cost[,] =
    let nodes = Nodes g
    let costs = Array2D.init nodes nodes (fun f t -> match (f,t) with
                                                     | (_,_) when f = t -> 0.0
                                                     | (_,_) when g.[f].ContainsKey t -> g.[f].[t]
                                                     | _ -> System.Double.PositiveInfinity)
    for m = 0 to (nodes - 1) do
        for f = 0 to (nodes - 1) do
            for t = 0 to (nodes - 1) do
                costs.[f,t] <- min  costs.[f, t] (costs.[f,m] + costs.[m,t])
            done
        done
    done
    costs

let n,m = Console.ReadLine() |> ParseInt2
let cs = ReadLinesN m
         |> Seq.toList
         |> List.map (ParseInt3 >> fun (x,y,c) -> (x-1,y-1,float c))
         |> DGraph n
         |> WarshallFloyd
let q = Console.ReadLine() |> ParseInt1
ReadLinesN q
|> Seq.map (ParseInt2 >> fun (f,t) -> cs.[f-1,t-1])
|> Seq.map (fun v -> if v = System.Double.PositiveInfinity then -1 else (int v))
|> Seq.iteri (fun i x -> if i <> (q-1) then printfn "%d" x else printf "%d" x)
