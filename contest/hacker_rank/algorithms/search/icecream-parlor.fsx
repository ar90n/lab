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

let BinarySearch (haystack : ('a * 'b) array) (needle : 'a) : int option =
    let (lo,hi) = (0, Array.length haystack - 1)
    let rec BinarySearch' (lo : int) (hi : int) =
        if hi < lo then None
        else
            let mid = lo + (hi - lo) / 2
            let pivot = fst  (haystack.[mid])
            if pivot > needle then BinarySearch' lo (mid - 1)
            else if pivot < needle then BinarySearch' (mid+1) hi
            else Some mid
    BinarySearch' lo hi


let t = Console.ReadLine() |> ParseInt1
for i = 1 to t do
    let m = Console.ReadLine() |> ParseInt1
    let n = Console.ReadLine() |> ParseInt1
    Console.ReadLine()
    |> ParseIntN
    |> Seq.toArray
    |> Array.mapi (fun i x -> (x,i+1))
    |> Array.sortBy (fun (x,_) -> x)
    |> (fun xs -> Array.map (fun x -> (x, (Array.filter (fun y -> y <> x) xs)) )  xs)
    |> Array.map (fun ((c,i),xs) -> (i,BinarySearch xs (m - c) |> function | None -> -1
                                                                           | Some(v) -> snd(xs.[v])))
    |> Array.filter (fun (_,x) -> 0 <= x)
    |> (Array.head >> (fun (a,b) -> printfn "%d %d" (min a b) (max a b)))
done
