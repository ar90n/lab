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


let (n,a,b) = Console.ReadLine() |> ParseInt3
ReadLinesN n
|> Seq.map (fun s -> s.Split(' ') |> function
                                     | [|"West"; l |] -> (System.Int32.Parse >> max a >> min b >> (~-)) l
                                     | [|"East"; l |] -> (System.Int32.Parse >> max a >> min b) l
                                     | _ -> failwith "error" )
|> Seq.sum
|> function
   | a when a < 0 -> sprintf "West %d" <| abs a
   | a when a > 0 -> sprintf "East %d" <| abs a
   | _ -> "0"
|> Console.WriteLine

