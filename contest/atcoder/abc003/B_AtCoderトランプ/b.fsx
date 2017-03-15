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

let s = Console.ReadLine() |> String.explode
let t = Console.ReadLine() |> String.explode

List.zip s t
|> List.filter (function
                | (a,b) when a = b -> false
                | ('@', a)  when (List.contains a (String.explode "atcoder")) -> false
                | (a, '@')  when (List.contains a (String.explode "atcoder")) -> false
                | _ -> true)
|> List.length
|> function
   | 0 -> "You can win"
   | _ -> "You will lose"
|> Console.WriteLine
