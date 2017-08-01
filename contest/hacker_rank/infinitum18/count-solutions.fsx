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

let ParseInt4 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2],arr.[3])

let ReadLinesN ( n : int ) =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.take n

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

let sqrti v =
    let m = v |> float |> sqrt |> int64
    if v = (m * m) then m
    else if v = ((m+1L) * (m+1L)) then m+1L
    else m-1L

let check a b d x =
    let tmp = b * b - 4L * ( x * x - a * x ) |> sqrti
    let z = ((b * b - 4L * (x * x - a * x)) - tmp * tmp) = 0L
    let y0 = (b + tmp) / 2L
    let y1 = (b - tmp) / 2L
    let r0 = z && (1L <= y0) && (y0 <= d)
    let r1 = (tmp <> 0L ) && z && (1L <= y1) && (y1 <= d)
    match (r0,r1) with
    | (true,true) -> 2
    | (true,false)
    | (false,true) -> 1
    | (false,false) -> 0

let q = Console.In.ReadLine() |> ParseInt1
for i = 1 to (int q) do
    let a,b,c,d = Console.In.ReadLine() |> ParseInt4
    [1L..c]
    |> List.map (check a b d)
    |> List.sum
    |> printfn "%d"
done


