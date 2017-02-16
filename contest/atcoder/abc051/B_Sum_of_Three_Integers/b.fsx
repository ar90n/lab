#!/usr/bin/env fsharpi

open System;

let readIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let readInt1 ( str : string ) =
    readIntN str |> fun arr -> arr.[0]

let readInt2 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1])

let readInt3 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let (k,s) = readInt2 <| Console.ReadLine()
seq{ for i = 0 to k do
        for j = 0 to k do
            yield (i,j) }
|> Seq.filter (fun (x,y) -> s - x - y <= k && 0 <= s - x - y)
|> fun s -> (sprintf "%d" ( Seq.length s ) )
|> Console.WriteLine

