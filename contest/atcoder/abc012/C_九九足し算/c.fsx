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
|> ParseInt1
|> ((-) 2025)
|> (fun r -> List.fold (fun acc a -> if (r % a = 0) && (0 < (r/a)) && ((r/a) < 10) then (a,r/a)::acc else acc ) [] [1..9])
|> List.map (fun (a,b) -> sprintf "%d x %d" a b)
|> List.sort
|> List.iter (printfn "%s")
