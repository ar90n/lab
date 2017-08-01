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
    let rec doit' xs ys n res =
        match (xs,ys) with
        | ([],[]) -> res
        | ([],[y]) -> res
        | ([x],[]) -> n
        | ((xh::xt),(yh::yt)) when xh = yh -> doit' xt yt (n+1) res
        | ((xa::xt),(ya::yb::yt)) when xa = yb -> doit' xs (yb::yt) n res
        | ((xa::xb::xt),(ya::yt)) when xb = ya -> doit' (xb::xt) ys (n+1) n
        | _ -> -1
    let rec doit'' xs ys n res =
        match (xs,ys) with
        | ([],[]) -> res
        | ([],[y]) -> res
        | ([x],[]) -> n
        | ((xh::xt),(yh::yt)) when xh = yh -> doit'' xt yt (n+1) res
        | ((xa::xb::xt),(ya::yt)) when xb = ya -> doit'' (xb::xt) ys (n+1) n
        | ((xa::xt),(ya::yb::yt)) when xa = yb -> doit'' xs (yb::yt) n res
        | _ -> -1
    max (doit' xs (List.rev xs) 0 -1) (doit'' xs (List.rev xs) 0 -1)

Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.map (String.explode >> doit)
|> Seq.iter (printfn "%d")
