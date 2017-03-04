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

let (w,h,n) = ParseInt3 <| Console.ReadLine()
ReadLinesN n
|> Seq.map ParseInt3
|> Seq.fold (fun (l,b,r,t) (x,y,a) ->  match a with
                                        | 1 -> (max x l,b,r,t)
                                        | 2 -> (l,b, min x r,t)
                                        | 3 -> (l,max b y,r,t)
                                        | _ -> (l,b,r,min y t)) (0, 0, w, h)
|> (fun (x0,y0,x1,y1) -> if (x0 < x1) && (y0 < y1) then (x1 - x0) * (y1 - y0) else 0)
|> printfn "%A"
