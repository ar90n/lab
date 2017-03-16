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

let n = Console.ReadLine() |> ParseInt1
let (a,b) = Console.ReadLine() |> ParseInt2
let k = Console.ReadLine() |> ParseInt1
Console.ReadLine()
|> ParseIntN
|> Set.ofSeq
|> Set.add a
|> Set.add b
|> Set.count
|> ((=) (k+2))
|> function
    | true -> "YES"
    | false -> "NO"
|> Console.WriteLine
