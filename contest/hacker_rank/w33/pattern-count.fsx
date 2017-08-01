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

let tail2 = function
    | (h::t) -> t
    | [] -> []

Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.toList
|> List.map (fun x -> x.Split('1')
                      |> Seq.toList
                      |> tail2
                      |> List.rev
                      |> tail2
                      |> List.rev
                      |> List.filter (fun x -> x <> "")
                      |> List.map (String.explode >> (List.forall (fun x -> x = '0')))
                      |> List.map (fun x -> if x then 1 else 0)
                      |> List.sum)
|> List.iter (printfn "%d")
