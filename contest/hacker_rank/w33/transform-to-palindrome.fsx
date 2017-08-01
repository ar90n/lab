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
            if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
            else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]


let doit xs = 
    let n = Array.length xs
    let dp = Array2D.create (n+1) (n+1) -1
    for i = 0 to n do
        dp.[i,0] <- 0
        dp.[0,i] <- 0
    done
    for i = 1 to n do
        for j = 1 to n do
            let c = if xs.[i-1] = xs.[n - j] then 1 else 0
            dp.[i,j] <- max (max (dp.[i-1,j-1] + c) dp.[i-1,j]) dp.[i,j-1]
        done
    done
    dp.[n,n]

let n,k,m = Console.ReadLine() |> ParseInt3
let uf = UnionFind.create (n+1)
ReadLinesN k
|> Seq.toList
|> List.map ParseInt2
|> List.iter (fun (f,t) -> UnionFind.unite uf f t)

Console.ReadLine()
|> ParseIntN
|> Seq.toArray
|> Array.map (fun x -> UnionFind.root uf x)
|> doit
|> printfn "%A"
