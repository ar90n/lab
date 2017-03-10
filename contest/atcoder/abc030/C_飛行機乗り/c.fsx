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

let (n,m) = Console.ReadLine() |> ParseInt2
let (x,y) = Console.ReadLine() |> ParseInt2
let ais = Console.ReadLine() |> ParseIntN |> Array.toList
let bis = Console.ReadLine() |> ParseIntN |> Array.toList

let rec a2b ais bis n c =
    match ais with
    | h :: t when n <= h -> b2a t bis (h + x) c
    | h :: t ->  a2b t bis n c
    | [] -> c
and b2a ais bis n c =
    match bis with
    | h :: t when n <= h -> a2b ais t (h + y) (c+1)
    | h :: t ->  b2a ais t n c
    | [] -> c

a2b ais bis 0 0 |> Console.WriteLine
