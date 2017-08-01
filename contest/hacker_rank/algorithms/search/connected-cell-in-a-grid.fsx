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
let m = Console.ReadLine() |> ParseInt1
let map = Array2D.zeroCreate n m
ReadLinesN n
|> Seq.map ParseIntN
|> Seq.iteri (fun i xs -> Array.iteri (fun j x -> map.[i,j] <- x ) xs)

let doit i j =
    let rec dfs i j = 
        if (0 <= i) && (i < n) && (0 <= j) && (j < m) then
            if (map.[i,j] = 1) then
                map.[i,j] <- 0
                1 + (dfs (i-1) (j-1)) + (dfs (i-1) j) + (dfs (i-1) (j+1)) + (dfs i (j-1)) + (dfs i (j+1)) + (dfs (i+1) (j-1)) + (dfs (i+1) j) + (dfs (i+1) (j+1))
            else
                0
        else
            0
    dfs i j

let r = ref 0
for i = 0 to (n-1) do
    for j = 0 to (m-1) do
        r := if map.[i,j] = 1 then max (doit i j) !r else !r
    done
done
printfn "%d" !r
