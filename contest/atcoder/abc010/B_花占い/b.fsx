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

let doit n =
    let rec doit' n c =
        if ( n % 2 <> 1) && (n % 3 <> 1) then c else doit' (n-1) (c+1)
    doit' (n-1) 0

Console.ReadLine() |> ignore

Console.ReadLine()
|> ParseIntN
|> Seq.map doit
|> Seq.reduce (+)
|> printfn "%d"
