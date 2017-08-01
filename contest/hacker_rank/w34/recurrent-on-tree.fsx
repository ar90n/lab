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

let Bfs (g : graph) (root : vertex) : (vertex * vertex) list = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let queue = new System.Collections.Generic.Queue< vertex * vertex >()
    queue.Enqueue (root, root)
    while( queue.Count <> 0 ) do
        let f,t = queue.Dequeue()
        if not visited.[t] then
            visited.[t] <- true
            g.[t].Keys
            |> Seq.filter (fun  t' -> not visited.[t'])
            |> Seq.map (fun t' -> (t,t'))
            |> Seq.iter queue.Enqueue
            if f <> t then result <- (f,t) :: result
    done

    result

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


let fibo = Array.create 100002 0L
fibo.[0] <- 0L
fibo.[1] <- 1L
for i = 2 to 100001 do
  fibo.[i] <- (fibo.[i-1] + fibo.[i - 2]) % 1000000007L
done

let n =  Console.ReadLine() |> ParseInt1
let parent = Array.init n (fun i -> i)
let isLeaf = Array.create n true
let memo = Array.create n 0L
let memo1 = Array.create n 0L
let queued = Array.create n false

let es = ReadLinesN (n-1)
         |> Seq.toList
         |> List.map (ParseInt2 >> (fun (a,b) -> (a-1,b-1,0)))
         |> Graph n
         |> (fun g -> Bfs g 0)
es
|> List.iter (fun (p,c) -> parent.[c] <- p)

let order = es
            |> List.map (fun (a,b) -> (a,b,0))
            |> DGraph n
            |> TopoSort
            |> function | Some vs -> vs
                        | None -> failwith "error"

let values = Console.ReadLine()
             |> ParseIntN
             |> Seq.toArray

let mutable l = 0L
order
|> List.iter (fun c -> let p = parent.[c]
                       let vc = values.[c]
                       let vp = values.[p]
                       l <- (l + fibo.[vc + 1]) % 1000000007L
                       memo.[c] <- (memo.[c] + fibo.[vc]) % 1000000007L
                       memo1.[c] <- (memo1.[c] + fibo.[vc+1]) % 1000000007L
                       //printfn "%d %d %d %A %A" c p vc memo memo1
                       if c <> p then
                         l <- (l + 2L * memo.[c]  * memo.[p]) % 1000000007L
                         l <- (l + 2L * memo1.[c] * memo1.[p]) % 1000000007L
                         //printfn "%A" l
                         l <- (l + 2L * fibo.[vp] * memo.[c]) % 1000000007L
                         l <- (l + 2L * fibo.[vp+1] * memo1.[c]) % 1000000007L
                         //printfn "%A" l
                         memo.[p] <- (memo.[p] + fibo.[abs (vp-1)] * memo.[c]) % 1000000007L
                         memo.[p] <- (memo.[p] + fibo.[vp] * memo1.[c]) % 1000000007L
                         memo1.[p] <- (memo1.[p] + fibo.[vp] * memo.[c]) % 1000000007L
                         memo1.[p] <- (memo1.[p] + fibo.[vp+1] * memo1.[c]) % 1000000007L
)

//printfn "%A" memo1
printfn "%d" l
