#!/usr/bin/env fsharpi

open System;

let ParseInt64N ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int64.Parse

let ParseInt643 ( str : string ) =
    ParseInt64N str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let (a,b,c) = ParseInt643 <| Console.ReadLine()
let res = (( ( a * b ) % 1000000007L ) * c) % 1000000007L
printfn "%d" res
