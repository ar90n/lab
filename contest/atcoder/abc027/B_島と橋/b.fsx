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
let a = Console.ReadLine() |> ParseIntN
Array.sum a
|> function
   | s when s % n <> 0 -> -1
   | s -> let avg = s / n
          Array.fold (fun (acc,l,b) v -> if acc = (avg * l) then (v,1,b) else (acc+v,l+1,b+1) ) (0,0,0) a
          |> fun (_,_,b) -> b
|> printfn "%d"
