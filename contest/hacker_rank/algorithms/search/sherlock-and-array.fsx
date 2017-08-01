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

let doit xs =
    let sum = List.sum xs
    xs
    |> List.scan (fun (l,r,y) x -> (l+y,r-x,x)) (0,sum,0)
    |> List.filter (fun (l,r,_) -> l = r)
    |> function | [] -> "NO"
                | _ -> "YES"

let t = Console.ReadLine() |> ParseInt1
for i = 0 to (t - 1)  do
    Console.ReadLine() |> ignore
    Console.ReadLine()
    |> ParseIntN
    |> Seq.toList
    |> doit
    |> printfn "%s"
done

