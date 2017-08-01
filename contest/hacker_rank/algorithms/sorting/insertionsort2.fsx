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

let ins l e =
    let rec doit acc rem =
        match rem with
        | [] -> List.rev (e :: acc)
        | (h::t) when h < e -> doit (h::acc) t
        | _ -> List.append (List.rev (e::acc)) rem
    doit [] l

let doit xs =
    let rec doit' acc rem =
        match rem with
        | [] -> []
        | (h::t) -> let acc' = ins acc h
                    (List.append acc' t)::(doit' acc' t )
    match xs with
    | [] -> []
    | (h::t) -> doit' [h] t

let n = Console.ReadLine() |> ParseInt1
Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> doit
|> List.map (fun xs -> List.map (sprintf "%d ") xs |> List.reduce (+))
|> List.iter (printfn "%s")
