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

let n,m = Console.ReadLine() |> ParseInt2
let dp = Array.create (n+1) 0L
dp.[0] <- 1L

Console.ReadLine()
|> ParseIntN
|> Array.toList
|> List.iter (fun c -> [c..n] |> List.iter (fun i -> dp.[i] <- dp.[i] + dp.[i - c]))
printfn "%d" dp.[n]
