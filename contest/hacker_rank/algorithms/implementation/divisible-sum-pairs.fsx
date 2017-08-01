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

let n,t = Console.ReadLine() |> ParseInt2
Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> List.mapi (fun i x -> (i,x))
|> (fun x -> List.collect (fun (i,y) -> List.map (fun (j,z) -> (i,j,y+z)) x) x)
|> List.filter (fun (a,b,c) -> (a < b) && ( c % t = 0) )
|> List.length
|> printfn "%d"
