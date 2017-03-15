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
let s = Console.ReadLine()
let rec doit cs i =
    if s = cs then i-1
    elif n < String.length cs then -1
    else let c = i % 3 
         match c with
         | 0 -> doit ("b" + cs + "b") (i + 1)
         | 1 -> doit ("a" + cs + "c") (i + 1)
         | _ -> doit ("c" + cs + "a") (i + 1)
doit "b" 1 |> printfn "%d"
