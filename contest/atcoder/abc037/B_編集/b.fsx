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

let (n,q) = ParseInt2 <| Console.ReadLine()
let mutable cnt = Array.create n 0
ReadLinesN q
|> Seq.map ParseInt3
|> Seq.iter (fun (l,r,t) -> List.iter (fun i -> cnt.[i] <- t) [(l-1)..(r-1)])

cnt
|> Array.iter (fun c -> printfn "%d" c)
