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

let WarshallFloyd (g : graph) : cost[,] =
    let nodes = Nodes g
    let costs = Array2D.init nodes nodes (fun f t -> match f,t with
                                                     | _ when f = t -> 0
                                                     | _ when g.[f].ContainsKey t -> g.[f].[t]
                                                     | _ -> (System.Int32.MaxValue / 2))
    for m = 0 to (nodes - 1) do
        for f = 0 to (nodes - 1) do
            for t = 0 to (nodes - 1) do
                costs.[f,t] <- min  costs.[f, t] (costs.[f,m] + costs.[m,t])
            done
        done
    done
    costs

let (n,m) = ParseInt2 <| Console.ReadLine()
let edges = ReadLinesN m |> Seq.map ParseInt3 |> Seq.map (fun (f,t,ci) -> (f-1,t-1,ci)) |> Seq.toList
let costs = edges |> Graph n |> WarshallFloyd
edges
|> List.filter (fun (f,t,c) -> costs.[f,t] <> c )
|> List.length
|> Console.WriteLine
