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

let (n,s,t) = Console.ReadLine() |> ParseInt3
ReadLinesN n
|> Seq.map ParseInt1
|> Seq.fold (fun acc v -> (List.head acc + v)::acc) [0]
|> (List.rev >> List.tail)
|> List.filter (fun v -> if s <= v && v <= t then true else false )
|> List.length
|> printfn "%d"
