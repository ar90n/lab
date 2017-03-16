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

module String =
    let explode (s:string) =
        [for c in s -> c]

    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

let rec doit (s: char array) (l,r) =
    if l < r then 
        let tmp = s.[l]
        s.[l] <- s.[r]
        s.[r] <- tmp
        doit s (l + 1, r - 1)

let cs = Console.ReadLine() |> String.explode |> List.toArray
Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.map (ParseInt2 >> fun (a,b) -> (a-1,b-1))
|> Seq.iter (doit cs)

cs
|> Array.toList
|> String.implode
|> Console.WriteLine
