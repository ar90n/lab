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

let s = Console.ReadLine()
        |> String.explode
        |> List.fold (fun acc x -> match acc with
                                   | [] -> [x] :: acc
                                   | hs::ts -> match hs with
                                               | h::t when h = x -> (x::hs) :: ts
                                               | _ -> [x] :: acc) []
        |> List.collect (List.mapi (fun i x -> (i+1) * ((int x) - (int 'a') + 1)))
        |> Set.ofList

Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.toList
|> List.map ParseInt1
|> List.map (fun x -> Set.contains x s)
|> List.map (fun x -> if x then "Yes" else "No")
|> List.iter (fun x -> printfn "%s" x)
