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

let ParseInt4 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2],arr.[3])

let ReadLinesN ( n : int ) =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.take n

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

let (a,b,c,k) = Console.ReadLine() |> ParseInt4
let (s,t) = Console.ReadLine() |> ParseInt2
let d = if (s + t) < k then 0 else c
let r = (a - d) * s + (b - d) * t
printfn "%d" r
