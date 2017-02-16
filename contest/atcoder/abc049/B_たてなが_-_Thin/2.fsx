#!/usr/bin/env fsharpi

open System;

let parseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let parseInt1 ( str : string ) =
    parseIntN str |> fun arr -> arr.[0]

let parseInt2 ( str : string ) =
    parseIntN str |> fun arr -> (arr.[0],arr.[1])

let parseInt3 ( str : string ) =
    parseIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let readLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

ignore(Console.ReadLine())
readLines 
|> Seq.map (fun a -> a + "\n" + a + "\n")
|> System.String.Concat
|> Console.Write
