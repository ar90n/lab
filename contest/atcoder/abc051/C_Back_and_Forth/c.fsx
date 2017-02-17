#!/usr/bin/env fsharpi

open System;

let readIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let readInt1 ( str : string ) =
    readIntN str |> fun arr -> arr.[0]

let readInt2 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1])

let readInt3 ( str : string ) =
    readIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let ps = readIntN <| Console.ReadLine()
let sx = ps.[0]
let sy = ps.[1]
let tx = ps.[2]
let ty = ps.[3]
let dist_x = abs <| tx - sx
let dist_y = abs <| ty - sy
let dx = if sx < tx then "R" else "L"
let idx = if sx > tx then "R" else "L"
let dy = if sy < ty then "U" else "D"
let idy = if sy > ty then "U" else "D"

[
    String.replicate dist_y dy;
    String.replicate dist_x dx;
    String.replicate dist_y idy;
    String.replicate (dist_x + 1) idx;
    String.replicate (dist_y + 1) dy;
    String.replicate (dist_x + 1) dx;
    idy;
    dx;
    String.replicate (dist_y + 1) idy;
    String.replicate (dist_x + 1) idx;
    dy;
]
|> String.concat ""
|> Console.WriteLine
