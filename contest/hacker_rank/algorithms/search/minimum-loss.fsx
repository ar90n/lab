#!/usr/bin/env fsharpi

open System;

let ParseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int64.Parse

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

let n = Console.ReadLine() |> ParseInt1
Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> List.mapi (fun i x -> (i,x))
|> List.sortBy (fun (_,x) -> x)
|> (fun xs -> List.zip (List.permute (fun i -> (i + 1) % (int32 n)) xs) xs)
|> List.skip 1
|> List.filter (fun ((i0,_),(i1,_)) -> i0 > i1)
|> List.minBy (fun ((_,a),(_,b)) -> b - a)
|> (fun ((_,a),(_,b)) -> printfn "%d" (b - a))

