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
///b = m - 2n - 2c
///a = 3n - m + c
/// -c <= 3n - m
///
///
/// c <= min (m - 2n) ((m / 2) - n)
/// max (m - 3n) ((m -3n)/2) <= c
let (n,m) = Console.ReadLine() |> ParseInt2
[0..n]
|> List.map (fun c -> (3 * n - m + c,m - 2 * n - 2 * c,c))
|> List.filter (fun (a,b,c) -> (0 <= a) && (0 <= b))
|> function
    | [] -> printfn "-1 -1 -1"
    | (a,b,c)::t ->printfn "%d %d %d" a b c
