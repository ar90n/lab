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

let (r,c,k) = Console.ReadLine() |> ParseInt3
let ks = Console.ReadLine()
         |> ParseInt1
         |> ReadLinesN
         |> Seq.map ParseInt2
         |> Seq.toList

let rs = Array.create r 0
let cs = Array.create c 0
ks
|> List.iter (fun s -> let r,c = s
                       rs.[r-1] <- rs.[r-1] + 1
                       cs.[c-1] <- cs.[c-1] + 1 )

let rmap = Array.create (k+1) 0
rs |> Array.iter (fun v -> if v <= k then rmap.[v] <- rmap.[v] + 1)

let cmap = Array.create (k+1) 0
cs |>  Array.iter (fun v -> if v <= k then cmap.[v] <- cmap.[v] + 1)

let r0 = List.fold (fun acc ra -> acc + rmap.[ra] * cmap.[k-ra] ) 0 [0..k]
let r1 = List.fold (fun acc (r,c) -> acc + if (rs.[r-1] + cs.[c-1]) = k then 1 else 0 ) 0 ks
let r2 = List.fold (fun acc (r,c) -> acc + if (rs.[r-1] + cs.[c-1]) = (k+1) then 1 else 0 ) 0 ks
let r3 = r0 - r1 + r2
printfn "%d" r3
