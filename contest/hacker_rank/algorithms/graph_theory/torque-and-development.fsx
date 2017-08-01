#!/usr/bin/env fsharpi

open System;
open System.IO;

let ParseIntN ( str : string ) =
    str.Split(' ')
    |> Array.map System.Int32.Parse

let ParseInt1 ( str : string ) =
    ParseIntN str |> fun arr -> arr.[0]

let ParseInt2 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1])

let ParseInt3 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2])

let ParseInt4 ( str : string ) =
    ParseIntN str |> fun arr -> (arr.[0],arr.[1],arr.[2],arr.[3])

let ReadLinesN ( n : int ) =
    fun _ -> System.Console.In.ReadLine()
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
            if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
            else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]

let q = Console.In.ReadLine() |> ParseInt1
for i = 1 to q do
    let n,m,cl,cr = Console.In.ReadLine() |> ParseInt4
    let uf = UnionFind.create n
    ReadLinesN m
    |> Seq.toList
    |> List.iter (ParseInt2 >> (fun (f,t) -> UnionFind.unite uf (f-1) (t-1)))
    [0..(n-1)]
    |> List.filter (fun i -> i = (UnionFind.root uf i))
    |> List.fold (fun acc i -> acc + (min ((int64 cl) * (int64 (UnionFind.count uf i))) ((int64 cr) * (int64 ((UnionFind.count uf i) - 1)) + (int64 cl)))) 0L
    |> (printfn "%d")
done
