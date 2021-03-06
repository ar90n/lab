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

let x = Console.ReadLine() |> ParseInt1
seq {(x+1) .. 999999}
|> Seq.skipWhile (fun y -> sprintf "%d" y
                           |> String.explode
                           |> List.map (fun x -> (int x) - (int '0'))
                           |> List.splitAt 3
                           |> (fun (a,b) -> (List.sum a, List.sum b))
                           |> (fun (a,b) -> a <> b))
|> Seq.head
|> printfn "%A"
