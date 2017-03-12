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

let n = Console.ReadLine() |> ParseInt1
let g = ReadLinesN (n-1)
        |> Seq.map (ParseInt2 >> fun (f,t) -> (f-1,t-1,0.0))
        |> Seq.toList
        |> Graph n
let q = Console.ReadLine() |> ParseInt1
ReadLinesN q
|> Seq.map (ParseInt2 >> (fun (s,d) -> Path g (s-1) (d-1)) >> function
                                                    | Some(l) -> List.length l
                                                    | None -> failwith "none" )
|> Seq.iter (printfn "%d")

