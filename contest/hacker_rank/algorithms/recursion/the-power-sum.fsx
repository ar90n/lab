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

let doit (n : int) (x : int) =
    let rec doit' (c : int) (x' : int) =
        let c' = pown c n
        match (c',x') with
        | _ when c' = x' -> 1
        | _ when x' < c' -> 0
        | _ -> (doit' (c+1) x') + (doit' (c+1) (x'-c'))
    doit' 1 x

let x = Console.ReadLine() |> ParseInt1
let n = Console.ReadLine() |> ParseInt1
doit n x |> printfn "%d"
