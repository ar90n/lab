#!/usr/bin/env fsharpi

open System;

let ParseInt64N ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int64.Parse

let ParseInt643 ( str : string ) =
    ParseInt64N str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let ReadLinesN ( n : int ) =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.take n

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

let (a,b,x) = ParseInt643 <| Console.ReadLine()
let res = (b / x) - (a / x) + if (a % x) = 0L then 1L else 0L
Console.WriteLine res
