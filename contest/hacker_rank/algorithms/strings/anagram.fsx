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

let doit xs =
    let n = List.length xs
    let a = List.take (n / 2) xs |> List.sort
    let b = List.skip (n / 2) xs |> List.sort
    let rec doit' a b r =
        match (a,b) with
        | ([],_) -> r
        | ((ah::at),[]) -> doit' at b (r+1)
        | ((ah::at),(bh::bt)) when ah < bh -> doit' at b (r+1)
        | ((ah::at),(bh::bt)) when bh < ah -> doit' a bt r
        | ((ah::at),(bh::bt)) -> doit' at bt r
    if (n % 2 = 1) then -1 else doit' a b 0

Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.map (String.explode >> doit)
|> Seq.iter (printfn "%d")
