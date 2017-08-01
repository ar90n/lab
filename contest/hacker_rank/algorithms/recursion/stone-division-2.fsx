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

let doit (memo : int64 array) n m =
    let ms = Array.length m
    let rec doit' i =
        if 0L <= memo.[i] then
            memo.[i]
        else
            memo.[i] <- [0..(ms-1)]
                        |> List.filter (fun j -> (m.[i] <> m.[j]) && ((m.[i] % m.[j]) = 0L))
                        |> List.map (fun j -> 1L + (m.[i] / m.[j]) * doit' j)
                        |> function | [] -> 0L
                                    | xs -> List.max xs
            memo.[i]

    [0..(ms-1)]
    |> List.filter (fun i -> (n <> m.[i]) && ((n % m.[i]) = 0L))
    |> List.map (fun i -> 1L + (n / m.[i]) * doit' i)
    |> function | [] -> 0L
                | xs -> List.max xs

let n = Console.ReadLine() |> ParseInt1 |> int
for i = 0 to (n-1)do
    let memo = Array.create 1001 -1L
    let n,_ = Console.ReadLine() |> ParseInt2
    Console.ReadLine()
    |> ParseIntN
    |> Seq.toArray
    |> doit memo n
    |> printfn "%d"
done
