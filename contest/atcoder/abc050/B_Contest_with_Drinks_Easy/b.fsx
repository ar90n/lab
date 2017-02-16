#!/usr/bin/env fsharpi

open System;

let readIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let readInt1 ( str : string ) =
    readIntN str |> fun arr -> arr.[0]

let readInt2 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1])

let readInt3 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let n = readInt1 <| Console.ReadLine()
let ts = readIntN <| Console.ReadLine()
let sum = Array.fold (+) 0 ts
let m = readInt1 <| Console.ReadLine()
fun _ -> Console.ReadLine()
|> Seq.initInfinite
|> Seq.take m
|> Seq.map readInt2
|> Seq.map (fun (p,x) -> sum - ts.[p-1] + x)
|> Seq.iter (fun r -> Console.WriteLine r)
