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

let t = Console.ReadLine() |> ParseInt1
let n = Console.ReadLine() |> ParseInt1
let an = Console.ReadLine() |> ParseIntN |> List.ofSeq
let m = Console.ReadLine() |> ParseInt1
let bn = Console.ReadLine() |> ParseIntN |> List.ofSeq

let rec doit an bn =
    match (an,bn) with
    | (_,[]) -> "yes"
    | ([],_) -> "no"
    | (ah::at,bh::bt) when (ah <= bh) && bh <= (ah + t) -> doit at bt
    | (ah::at,bh::bt) -> doit at (bh::bt)

doit an bn |> Console.WriteLine
