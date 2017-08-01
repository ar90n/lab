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


let doit n a b =
    let map = Array2D.create n n System.Int32.MaxValue
    let rec doit' i x y = 
        if (0 <= x) && (x < n) && (0 <= y) && (y < n) && (i < map.[x,y]) then
            map.[x,y] <- i
            doit' (i+1) (x + a) (y + b)
            doit' (i+1) (x + a) (y - b)
            doit' (i+1) (x - a) (y + b)
            doit' (i+1) (x - a) (y - b)
            doit' (i+1) (x + b) (y + a)
            doit' (i+1) (x + b) (y - a)
            doit' (i+1) (x - b) (y + a)
            doit' (i+1) (x - b) (y - a)
        else
            ()
    doit' 0 0 0
    map.[n-1,n-1] |> function | System.Int32.MaxValue -> -1
                              | x -> x

let n = Console.ReadLine() |> ParseInt1
[
    for i = 1 to (n-1) do
        for j = 1 to (n-1) do
            yield (i,j)
]
|> List.map (fun (i,j) -> doit n i j)
|> List.chunkBySize (n - 1)
|> List.map (List.map (sprintf "%d") >> List.reduce (fun acc x -> acc + " " + x))
|> List.iter (printfn "%s")
