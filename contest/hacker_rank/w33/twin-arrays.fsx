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

let doit a b =
    match (a,b) with
    | ((i,x)::t1, (j,y)::t2) when i <> j -> x + y
    | ((i,x)::(i',x')::t1, (j,y)::(j',y')::t2) -> if x' < y' then y + x' else x + y'
    | _ -> failwith "error"

Console.ReadLine()
|> ignore


ReadLinesN 2
|> Seq.toList
|> List.map (ParseIntN
             >> Seq.toList
             >> List.mapi (fun i x -> (i,x))
             >> List.sortBy (fun (_,x) ->  x))
|> function | (a::b::[]) -> doit a b
            | _ -> failwith "error"
|> printfn "%A"
