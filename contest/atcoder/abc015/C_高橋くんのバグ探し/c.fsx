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

let doit (table : (int array) array) =
    let rec doit' acc n =
        if n < Array.length table then
            Array.fold (fun a b -> a || (doit' (acc  ^^^ b) (n+1)) ) false table.[n]
        else
            acc = 0
    doit' 0 0

let (n,k) = Console.ReadLine() |> ParseInt2
ReadLinesN n
|> Seq.map ParseIntN
|> Seq.toArray
|> doit
|> function
    | true -> "Found"
    | false -> "Nothing"
|> Console.WriteLine
