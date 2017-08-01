#!/usr/bin/env fsharpi

open System;

let ParseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.UInt64.Parse

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

let rec doit n x =
    let rec doit' w y = 
        if w <> 0 then
            let w' = w / 2
            let y0 = y &&& ((1UL <<< w') - 1UL)
            let y1 = y >>> w'
            if y1 <> 0UL then w' + (doit' w' y1) else doit' w' y0
        else
            0
    match x with
    | 0UL -> if (n % 2) = 0 then "Louise" else "Richard"
    | _ -> let m = 1UL <<< (doit' 64 x)
           if x = m then doit (n+1) (x / 2UL)  else doit (n+1) (x - m)

let n = Console.ReadLine() |> ParseInt1 |> int
for i = 0 to (n-1) do
    Console.ReadLine()
    |> ParseInt1
    |> doit 0
    |> printfn "%s"
done
