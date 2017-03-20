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

let rec doit acc n l =
    if n = 0 then
        [acc]
    else
        match l with
        | [] -> []
        | h::t -> (doit acc n t) @ (doit (acc + h) (n-1) t)

Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> doit 0 3
|> Set.ofList
|> Set.toList
|> List.sortDescending
|> List.skip 2
|> List.head
|> printfn "%A"
