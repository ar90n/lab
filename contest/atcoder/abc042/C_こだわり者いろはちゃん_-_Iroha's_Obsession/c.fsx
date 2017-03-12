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

let ds = Array.create 10 true
let (n,k) = Console.ReadLine() |> ParseInt2
Console.ReadLine()
|> ParseIntN
|> Array.iter (fun d -> ds.[d] <- false)

let isOk v =
    let rec split v acc =
        match v with
        | 0 when List.length acc = 0 -> [0]
        | 0 -> acc
        | _ -> split ( v / 10 ) ( (v % 10 ) :: acc )
    split v []
    |> List.fold (fun acc v -> acc && ds.[v]) true

fun i -> n + i
|> Seq.initInfinite
|> Seq.skipWhile (fun v -> (isOk >> not) v)
|> Seq.head
|> printfn "%A"
