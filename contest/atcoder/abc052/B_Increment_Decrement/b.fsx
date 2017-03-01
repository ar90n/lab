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
    /// Converts a string into a list of characters.
    let explode (s:string) =
        [for c in s -> c]

    /// Converts a list of characters into a string.
    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

Console.ReadLine() |> ignore
Console.ReadLine()
|> String.explode
|> List.fold (fun (acc,mv) c -> let nacc = acc + if c = 'I' then 1 else -1
                                (nacc, max mv nacc)) (0,0)
|> fun (_,mv) -> Console.WriteLine (sprintf "%d" mv )
