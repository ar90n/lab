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

let doit xs =
    let rec doit' acc rem v =
        match rem with
        | (h::t) when v < h -> ((h::h::acc),t)::(doit' (h::acc) t v)
        | _ -> [((v::acc),rem)]
    match xs with
    | [] -> []
    | (h::t) -> doit' [] t h


let n = Console.ReadLine() |> ParseInt1
Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> List.rev
|> doit
|> List.map (fun (acc,rem) -> List.rev (List.append (List.rev acc) rem))
|> List.map (fun xs -> List.map (sprintf "%d ") xs |> List.reduce (+))
|> List.iter (printfn "%s")
