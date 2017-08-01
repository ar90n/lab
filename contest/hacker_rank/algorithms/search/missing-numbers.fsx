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


Console.ReadLine() |> ignore
let a = Array.zeroCreate 10001
Console.ReadLine()
|> ParseIntN
|> Seq.iter (fun x -> a.[x] <- a.[x] + 1)

Console.ReadLine() |> ignore
let b = Array.zeroCreate 10001
Console.ReadLine()
|> ParseIntN
|> Seq.iter (fun x -> b.[x] <- b.[x] + 1)

[0..10000]
|> List.filter (fun i -> a.[i] <> b.[i])
|> List.map (sprintf "%d")
|> List.reduce (fun acc x -> acc + " " + x)
|> printfn "%s"
