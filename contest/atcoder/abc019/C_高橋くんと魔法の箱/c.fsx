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

let found = new System.Collections.Generic.HashSet< int >()
let n = Console.ReadLine() |> ParseInt1
Console.ReadLine()
|> ParseIntN
|> Seq.sort
|> Seq.fold (fun acc a -> (found.Add >> ignore) (2 * a)
                          if found.Contains a then
                              acc
                          else
                              (found.Add >> ignore) a
                              acc + 1) 0
|> printfn "%d"
