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

let (n,k) = Console.ReadLine() |> ParseInt2
Console.ReadLine()
|> ParseIntN
|> Array.zip [|1L..n|]
|> Array.map ( fun (i,a) -> min k i |> min (n - i + 1L) |> min (n - k + 1L) |> ((*) a) )
|> Array.fold (+) 0L
|> Console.WriteLine
