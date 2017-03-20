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

let shift (l :int array) =
    let tmp = l.[0]
    List.iter (fun i -> l.[i] <- l.[i+1]) [0..4]
    l.[5] <- tmp

let flip (l : int array) (i : int) =
    let tmp = l.[i]
    l.[i] <- l.[i+1]
    l.[i+1] <- tmp

let n = Console.ReadLine() |> ParseInt1
let flips = n % 5
let shifts = (n / 5) % 6

let l = [|1..6|]
[1..shifts] |> List.iter (fun _ -> shift l)
[0..(flips-1)] |> List.iter (fun i -> flip l i)
l |> Array.iter (fun v -> printf "%d" v)
printf "\n"
