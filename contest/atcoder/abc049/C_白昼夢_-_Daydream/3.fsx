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

let rec parse ( v: char list) =
    match v with
    | 'd'::'r'::'e'::'a'::'m'::'e'::'r'::'d'::remain -> parse ('d'::remain)
    | 'd'::'r'::'e'::'a'::'m'::'e'::'r'::'e'::remain -> parse ('e'::remain)
    | 'd'::'r'::'e'::'a'::'m'::'e'::'r'::[] -> parse []
    | 'e'::'r'::'a'::'s'::'e'::'r'::remain -> parse remain
    | 'd'::'r'::'e'::'a'::'m'::remain -> parse remain
    | 'e'::'r'::'a'::'s'::'e'::remain -> parse remain
    | [] -> ""
    | _ -> "."

Console.ReadLine()
|> Seq.toList
|> fun s -> if (parse s) = "" then "YES" else "NO"
|> Console.WriteLine
