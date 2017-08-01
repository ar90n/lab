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
    let rec adj =
        function
        | [] -> []
        | [x] -> []
        | (x::y::t) -> (x,y):: (adj (y::t))
    List.zip (adj xs) (adj <| List.rev xs)
    |> List.map (fun ((a,b),(c,d)) -> ((int b) - (int a),(int d) - (int c)))
    |> List.map (fun (a,b) ->  (abs a)  = (abs b))
    |> List.forall ((=)true)
    |> function | true -> "Funny"
                | false -> "Not Funny"

Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.map String.explode
|> Seq.map doit
|> Seq.iter (printfn "%s")

