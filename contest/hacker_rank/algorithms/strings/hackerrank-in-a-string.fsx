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

module String =
    let explode (s:string) =
        [for c in s -> c]

    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

let doit s =
    let rec doit' s' l' =
        match (s',l') with
        | (_,[]) -> "YES"
        | ([],_) -> "NO"
        | (sh::st, lh::lt) when sh = lh -> doit' st lt
        | (sh::st, _) -> doit' st l'
    doit' (String.explode s) (String.explode "hackerrank")

let n = Console.ReadLine() |> ParseInt1
ReadLinesN n
|> Seq.toList
|> List.map doit
|> List.iter (printfn "%s")
