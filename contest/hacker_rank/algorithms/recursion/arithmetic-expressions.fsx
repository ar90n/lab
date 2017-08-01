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

let doit l =
    let combine ope =
        l
        |> List.map (sprintf "%d")
        |> function | h::t -> h :: (List.zip ope t |> List.map (fun (a,b) -> a+b))
                    | [] -> []
        |> List.reduce (fun acc x -> acc + x)
    let rec doit' l' s acc =
        match l' with
        | [] when s % 101 = 0 -> List.rev acc |> combine |> failwith
        | [] -> ()
        | h::t -> doit' t ((s * h) % 101) ("*" :: acc)
                  doit' t ((s + h) % 101) ("+" :: acc)
                  doit' t ((s - h) % 101) ("-" :: acc)
    try
        l |> function | h::t -> doit' t h []
                      | [] -> printfn ""
    with
    | Failure x -> printfn "%s" x

Console.ReadLine()
|> ignore

Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> doit
