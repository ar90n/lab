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

module UnionFind =
    type _unionfind = int[] * int[]

    let create (n : int) : _unionfind =
        let id = Array.init n (fun i -> i)
        let sz = Array.create n 1
        (id,sz)

    let count (uf : _unionfind) ( i : int ) =
        uf |> function | (_,sz) -> sz.[i]

    let rec root ( uf : _unionfind ) ( i : int ) =
        let (id,sz) = uf
        if i <> id.[i] then id.[i] <- root uf id.[i]
        id.[i]

    let connected ( uf : _unionfind ) ( p : int ) ( q : int ) =
        (root uf p) = (root uf q)

    let unite ( uf : _unionfind ) ( p : int ) ( q : int ) =
        let i = root uf p
        let j = root uf q
        let (id,sz) = uf
        if i <> j then
            if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]; sz.[i] <- 0
            else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]; sz.[j] <- 0

let n,p = Console.ReadLine() |> ParseInt2
let uf = UnionFind.create n
ReadLinesN p
|> Seq.toList
|> List.map ParseInt2
|> List.iter (fun (a,b) -> UnionFind.unite uf a b)
let sz = uf |> function | (_, sz) -> sz
let sum = Array.sum sz
sz
|> Array.map (fun x -> x * (sum- x) |> int64)
|> Array.sum
|> (fun x -> x / 2L)
|> printfn "%d"
