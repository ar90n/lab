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

let rec Gcd a b =
    let (minv, maxv) = (min a b, max a b)
    if minv = 0 then maxv else Gcd (maxv % minv) minv

let Lcm a b =
    a * b / (Gcd a b)

let n = ParseInt1 <| Console.ReadLine()
ReadLinesN n
|> Seq.map ParseInt2
|> Seq.fold (fun (va,vb) (ra,rb) -> let gcd = Gcd rd rb
                                    let ra,rb = (ra / gcd, rb / gcd)
                                    let ga,gb = (va / ra, vb / rb ) 
                                    ) (1,1)
