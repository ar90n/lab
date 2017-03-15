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

let n = Console.ReadLine() |> ParseInt1
seq {
    yield 0
    yield 0
    yield 1
    yield! Seq.unfold (fun st -> let a,b,c = st
                                 let n = (a + b + c) % 10007
                                 Some(n,(b,c,n))) (0,0,1)
}
|> Seq.skip (n-1)
|> Seq.head
|> printfn "%d"
