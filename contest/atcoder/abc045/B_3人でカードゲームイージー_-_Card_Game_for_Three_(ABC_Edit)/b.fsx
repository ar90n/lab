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

let rec doit n sa sb sc =
    match (sa,sb,sc) with
    | ([],_,_) when n = 0 -> "A"
    | (h::t, _, _) when n = 0 -> doit h t sb sc
    | (_,[],_) when n = 1 -> "B"
    | (_, h::t, _) when n = 1 -> doit h sa t sc
    | (_,_,[]) when n = 2 -> "C"
    | (_, _, h::t) when n = 2 -> doit h sa sb t
    | _ -> "Error"

let sa = Console.ReadLine() |> String.explode |> List.map (fun c -> (int c) - (int 'a'))
let sb = Console.ReadLine() |> String.explode |> List.map (fun c -> (int c) - (int 'a'))
let sc = Console.ReadLine() |> String.explode |> List.map (fun c -> (int c) - (int 'a'))
doit 0 sa sb sc |> Console.WriteLine
