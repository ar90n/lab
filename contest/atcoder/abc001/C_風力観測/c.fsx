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
|> ParseInt2
|> function
    | (d,w) when w < 15 -> (d,0)
    | (d,w) when w < 93 -> (d,1)
    | (d,w) when w < 201 -> (d,2)
    | (d,w) when w < 327 -> (d,3)
    | (d,w) when w < 477 -> (d,4)
    | (d,w) when w < 645 -> (d,5)
    | (d,w) when w < 831 -> (d,6)
    | (d,w) when w < 1029 -> (d,7)
    | (d,w) when w < 1245 -> (d,8)
    | (d,w) when w < 1467 -> (d,9)
    | (d,w) when w < 1707 -> (d,10)
    | (d,w) when w < 1959 -> (d,11)
    | (d,w) -> (d,12)
|> function
    | (_,0) -> ("C",0)
    | (d,w) when d < 113 -> ("N",w)
    | (d,w) when d < 338 -> ("NNE",w)
    | (d,w) when d < 563 -> ("NE",w)
    | (d,w) when d < 788 -> ("ENE",w)
    | (d,w) when d < 1013 -> ("E",w)
    | (d,w) when d < 1238 -> ("ESE",w)
    | (d,w) when d < 1463 -> ("SE",w)
    | (d,w) when d < 1688 -> ("SSE",w)
    | (d,w) when d < 1913 -> ("S",w)
    | (d,w) when d < 2138 -> ("SSW",w)
    | (d,w) when d < 2363 -> ("SW",w)
    | (d,w) when d < 2588 -> ("WSW",w)
    | (d,w) when d < 2813 -> ("W",w)
    | (d,w) when d < 3038 -> ("WNW",w)
    | (d,w) when d < 3263 -> ("NW",w)
    | (d,w) when d < 3488 -> ("NNW",w)
    | (d,w) -> ("N",w)
|> (fun (d,w) -> printfn "%s %d" d w)
