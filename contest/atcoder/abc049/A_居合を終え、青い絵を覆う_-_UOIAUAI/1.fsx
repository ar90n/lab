#!/usr/bin/env fsharpi

open System;

let readIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let readInt1 ( str : string ) =
    readIntN str |> fun arr -> arr.[0]

let readInt2 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1])

let readInt3 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

Console.ReadLine() |> (fun c -> if List.contains c ["a";"i";"u";"e";"o"] then "vowel" else "consonant") |> Console.WriteLine
