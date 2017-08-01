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

let doit = function
           | [] -> 0
           | (h::t) -> List.fold (fun (lo,hi,p) x -> (max lo (hi + p - 1), max (lo + x - 1) (hi + abs (x - p)), x)) (0, 0, h) t
                       |> (fun (lo,hi,_) -> max lo hi)

let n = Console.ReadLine() |> ParseInt1
for i = 1 to n do
    Console.ReadLine() |> ignore
    Console.ReadLine()
    |> ParseIntN
    |> Seq.toList
    |> doit
    |> printfn "%A"
done
