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

let rec powMod (a: int64) (b : int64) (c : int64) =
    if b = 0L then 1L
    else if ( b % 2L ) = 1L then ( a * ( powMod a (b - 1L) c ) ) % c
    else 
        let d = powMod a (b / 2L ) c
        ( d * d ) % c

let check1 ( l : int array ) =
    let offset = l.Length % 2
    if offset = 1 && l.[0] <> 0 then false else true

let check2 ( l : int array ) =
    let offset = l.Length % 2
    let evens = seq { for i in offset .. 2 .. l.Length - 1 -> l.[i] }
    let odds = seq { for i in offset + 1 .. 2 .. l.Length - 1 -> l.[i] }
    Seq.fold (&&) true ( Seq.zip evens odds |> Seq.map (fun (a,b) -> a =b ) )

let check3 ( l : int array ) =
    let offset = l.Length % 2
    let v = seq { for i in offset .. 2 .. l.Length - 1 -> l.[i] }
    let e = seq { for i in offset + 1 .. 2 .. l.Length - 1 -> i }
    Seq.fold (&&) true ( Seq.zip v e |> Seq.map (fun (a,b) -> a = b ) )

let calc ( l : int array ) =
    let width : int64 = int64( l.Length - ( l.Length % 2 ) )
    powMod 2L ( width / 2L ) 1000000007L

let n = readInt1 <| Console.ReadLine()
let v = Array.sort ( readIntN <| Console.ReadLine() )
let res = if (check1 v) && (check2 v) && (check3 v) then ( calc v ) else 0L
Console.WriteLine res



