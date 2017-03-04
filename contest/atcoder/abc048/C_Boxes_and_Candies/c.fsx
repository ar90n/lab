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


let (n,x) = ParseInt2 <| Console.ReadLine()
let cs = ParseIntN <| Console.ReadLine()
let work = Array.copy cs

work.[0] <- min work.[0] x
[1..n-1] |> List.iter (fun i -> work.[i] <- work.[i] + min 0 (x - (work.[i-1] + work.[i])))

Array.zip cs work
|> Array.fold (fun acc (a,b) -> acc + (abs >> int64)(a-b)) 0L
|> Console.WriteLine
