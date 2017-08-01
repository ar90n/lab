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

let splitBy2 f xs =
    List.fold (fun acc x -> match acc with
                            | [] -> [[x]]
                            | (h::t) when  f (List.head h) x -> [x]::h::t
                            | (h::t) -> (x::h)::t) [] xs
    |> List.rev

Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.toList
|> List.map (String.explode >> splitBy2 (<>) >> List.map (fun xs -> (List.length xs) - 1) >> List.sum)
|> List.iter (printfn "%d")
