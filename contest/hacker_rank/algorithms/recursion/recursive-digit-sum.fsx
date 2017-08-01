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

let doit sn k =
    let calc l =
        l
        |> List.map (fun x -> (int x) - (int '0'))
        |> List.sum
    let rec doit' sn =
        sn
        |> String.explode |> function | [x] -> (int x) - (int '0')
                                      | xs -> (calc >> (sprintf "%d") >> doit') xs
    sn
    |> String.explode
    |> calc
    |> (fun x -> k * (int64 x))
    |> (sprintf "%d")
    |> doit'

let sn,sk = Console.ReadLine().Split(' ') |> (fun xs -> (xs.[0],xs.[1]))
let k = (ParseInt1 >> int64) sk
doit sn k |> (printfn "%d")
