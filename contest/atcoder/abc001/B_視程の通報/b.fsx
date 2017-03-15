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
    | v when v < 100 -> "00"
    | v when v <= 5000 -> sprintf "%02d" (v / 100)
    | v when 6000 <= v && v <= 30000 -> sprintf "%d" <| v / 1000 + 50
    | v when 35000 <= v && v <= 70000 -> sprintf "%d" <| (v / 1000 - 30) / 5 + 80
    | v when 70000 < v -> "89"
    | _ -> failwith "error"
|> Console.WriteLine
