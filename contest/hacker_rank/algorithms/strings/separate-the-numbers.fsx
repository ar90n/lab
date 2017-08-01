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
    let rec sp acc l =
        match l with
        | [] -> []
        | (h::t) -> let acc' = 10L * acc + (int64 h)
                    (acc',t)::(sp acc' t)
    let rec doit f p xs =
        match xs with
        | [] -> Some f
        | [h] when (p+1L) = h -> Some f
        | [h] -> None
        | (h::t) when h = 0L ->  None
        | _ -> sp 0L xs
               |> List.filter (fun (p',_) -> (p+1L) = p')
               |> List.map (fun (p',t) -> doit f p' t)
               |> List.tryFind (fun x -> x <> None)
               |> function | None -> None
                           | Some y -> y
    sp 0L xs
    |> List.rev
    |> List.tail
    |> List.rev
    |> List.map (fun (p,xs') -> doit p p xs')
    |> List.tryFind ((<>) None)
    |> function | None -> None
                | Some y -> y
Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.map (String.explode >> (List.map (fun x -> (int64 x) - (int64 '0'))) >> doit)
|> Seq.map (fun y -> match y with
                     | None -> "NO"
                     | Some x -> sprintf "YES %d" x)
|> Seq.iter (printfn "%s")
