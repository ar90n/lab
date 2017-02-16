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

let s = Console.ReadLine()
let ss = s.Split( ' ' )
let aa = System.Int32.Parse ss.[0]
let bb = System.Int32.Parse ss.[2]
let res = if ss.[1] = "+" then  aa + bb else aa - bb
Console.WriteLine res

