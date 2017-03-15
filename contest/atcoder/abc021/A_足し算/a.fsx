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

Console.ReadLine()
|> ParseInt1
|> function
    | 1 -> [1;1]
    | 2 -> [1;2]
    | 3 -> [2;1;2]
    | 4 -> [1;4]
    | 5 -> [2;1;4]
    | 6 -> [2;2;4]
    | 7 -> [3;1;2;4]
    | 8 -> [1;8]
    | 9 -> [2;1;8]
    |10 -> [2;2;8]
    | _ -> failwith "error"
|> List.iter (printfn "%d")
