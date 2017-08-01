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

let doit xs m =
    xs
    |> List.map (fun x -> x - m )
    |> List.map (fun x -> ((x / 5),((x % 5) / 2),((x % 5) % 2)))
    |> List.map (fun (a,b,c) -> a + b + c)
    |> List.sum

let n = Console.ReadLine() |> ParseInt1
for i = 1 to n do
    Console.ReadLine() |> ignore
    Console.ReadLine()
    |> ParseIntN
    |> Seq.toList
    |> (fun x -> let m = List.min x
                 [(m-5); (m-2); (m-1);m ]
                 |> List.filter (fun x -> 0 <= x)
                 |> List.map (doit x))
    |> List.min
    |> (printfn "%d")
done
