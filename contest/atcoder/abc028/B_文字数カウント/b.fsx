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

module String =
    let explode (s:string) =
        [for c in s -> c]

    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

Console.ReadLine()
|> String.explode
|> List.fold ( fun (a,b,c,d,e,f) cc -> match cc with
                                       | 'A' -> (a+1,b,c,d,e,f)
                                       | 'B' -> (a,b+1,c,d,e,f)
                                       | 'C' -> (a,b,c+1,d,e,f)
                                       | 'D' -> (a,b,c,d+1,e,f)
                                       | 'E' -> (a,b,c,d,e+1,f)
                                       | 'F' -> (a,b,c,d,e,f+1)) (0,0,0,0,0,0)
|> fun (a,b,c,d,e,f) -> printfn "%d %d %d %d %d %d" a b c d e f
