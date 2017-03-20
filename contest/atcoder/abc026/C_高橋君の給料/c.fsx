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

let salary (a,b) = a + b + 1

let n = Console.ReadLine() |> ParseInt1
let s = Array.create n (System.Int32.MaxValue,-System.Int32.MaxValue)
ReadLinesN (n-1)
|> Seq.map ParseInt1
|> Seq.zip [2..n]
|> Seq.rev
|> Seq.iter (fun (i,b) -> s.[b-1] <- (min (fst s.[b-1]) (salary s.[i-1]), max (snd s.[b-1]) (salary s.[i-1]))) 

let r = salary s.[0]
printfn "%d" r
