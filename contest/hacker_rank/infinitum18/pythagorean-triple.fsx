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

Console.ReadLine()
|> ParseInt1
|> function | x when (x % 2L = 0L ) -> (x / 2L, 1L)
            | x -> ( ( x + 1L ) / 2L,( x - 1L ) / 2L )
|> (fun (m,n) -> (m * m - n * n, 2L * m * n, m * m + n * n))
|> (fun (a,b,c) -> printfn "%d %d %d" a b c)
