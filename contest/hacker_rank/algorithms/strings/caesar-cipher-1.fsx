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
|> ignore

let s = Console.ReadLine()
let n = Console.ReadLine() |> ParseInt1
s
|> String.explode
|> List.map int
|> List.map (fun x -> match x with
                      | _ when ((int 'a') <= x) && (x <= (int 'z')) -> (x - (int 'a') + n) % 26 + (int 'a')
                      | _ when ((int 'A') <= x) && (x <= (int 'Z')) -> (x - (int 'A') + n) % 26 + (int 'A')
                      | _ -> x )
|> List.map char
|> String.implode
|> printf "%s"
