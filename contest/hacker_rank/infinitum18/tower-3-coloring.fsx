#!/usr/bin/env fsharpi

open System;

let ParseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int64.Parse

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

let rec PowMod (a: int64) (b : int64) (c : int64) =
    if b = 0L then 1L
    else if ( b % 2L ) = 1L then ( a * ( PowMod a (b - 1L) c ) ) % c
    else 
        let d = PowMod a (b / 2L) c
        ( d * d ) % c

let m = 1000000007L

Console.ReadLine()
|> ParseInt1
|> (fun x -> PowMod 3L x (m-1L))
|> (fun x -> PowMod 3L x m)
|> printfn "%d"
