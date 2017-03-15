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

let ReadLinesN ( n : int ) =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.take n

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)


let PI : double = 2.0 * asin(1.0)
module List =
    let evenodd a = List.foldBack (fun x (l,r) -> x::r,l) a ([],[])

let n = Console.ReadLine() |> System.Int32.Parse
ReadLinesN n
|> Seq.toList
|> List.map (ParseInt1 >> fun r -> r * r)
|> List.sortWith (fun a b -> compare b a)
|> List.evenodd
|> (fun (e,o) -> (List.sum e) - (List.sum o) |> double |> ((*) PI))
|> Console.WriteLine
