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

let (n,a) = Console.ReadLine() |> ParseInt2
let xs = Console.ReadLine() |> ParseIntN
let dp : int64[,,] = Array3D.zeroCreate (n+1) (n+1) (n * 50 + 1)

for k = 0 to n * 50 do
    for j = 0 to n do
        for i = 0 to n do
            dp.[i,j,k] <- match (i,j,k) with
                          | (0,0,0) -> 1L
                          | (i,j,k) when 0 < i && k < xs.[i-1] -> dp.[i-1,j,k]
                          | (i,j,k) when 0 < i && 0 < j && xs.[i-1] <= k -> dp.[i-1,j,k] + dp.[i-1,j-1,k- xs.[i-1]]
                          | _ -> 0L
        done
    done
done
[1..n]
|> List.fold (fun acc i -> acc + dp.[n,i,i*a]) 0L
|> printfn "%d"
