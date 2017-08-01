#!/usr/bin/env fsharpi

open System;

let ParseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int64.Parse

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

let doit xs =
    let n = List.length xs
    xs
    |> List.mapi (fun i x -> List.fold (fun acc _ -> x :: acc) [] [0..(n - i - 1)]
                             |> (String.implode >> System.Int64.Parse)
                             |> (fun y -> ((int64 i)+1L) * y))
    |> List.reduce (fun acc x -> (acc + x ) % 1000000007L)

Console.ReadLine()
|> String.explode
|> List.map (fun x -> (int64 x) - (int64 '0'))
|> List.fold (fun (a,b,c) x -> (a + (x * c), (10L * b + a + (x * c)) % 1000000007L, c+1L)) (0L,0L,1L)
|> (fun (_,x,_) -> x)
|> printfn "%d"
