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

Console.ReadLine()
|> (fun s -> s.IndexOf  "WBWBWBW" )
|> (fun i -> i % 12)
|> function 
    | 0 -> "Fa"
    | 1 -> "Mi"
    | 3 -> "Re"
    | 5 -> "Do"
    | 6 -> "Si"
    | 8 -> "La"
    | 10 -> "So"
    | _ -> failwith "error"
|> Console.WriteLine
