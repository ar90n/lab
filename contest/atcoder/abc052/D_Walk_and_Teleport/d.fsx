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

let (n',a',b') = ParseInt3 <| Console.ReadLine()
let (n,a,b) = (int64 n', int64 a', int64 b')
let (_,ac) = Console.ReadLine()
             |> ParseIntN
             |> Array.toList
             |> List.map int64
             |> (fun ls -> List.fold (fun (p,ac) c -> (c, ((min ((c-p) * a) b) + ac))) ((List.head ls),0L) (List.tail ls))
printfn "%d" ac
