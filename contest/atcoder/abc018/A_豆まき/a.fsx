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

ReadLinesN 3
|> Seq.toList
|> List.map ParseInt1
|> List.zip [1;2;3]
|> List.sortWith (fun a b -> compare (snd b) (snd a))
|> List.zip [1;2;3]
|> List.sortWith (fun a b -> compare ((snd >>fst) a) ((snd >> fst) b))
|> List.map fst
|> List.iter (printfn "%d")
