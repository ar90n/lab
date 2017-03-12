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
Console.ReadLine()
|> ParseIntN
|> Array.toList
|> function
    | f::s::[] -> abs (s - f)
    | f::s::t -> t
                 |> List.fold (fun ((p2,c2),(p1,c1)) a -> ((p1,c1),(a,min (c2 + abs (a - p2)) (c1 + abs (a - p1)) ))) ((f,0),(s,abs(s-f)))
                 |> (snd >> snd)
    | _ -> failwith "error"
|> printfn "%A"
