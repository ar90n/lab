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


type UnionFind (N) =
    let id : int[] = Array.init N (fun i -> i )
    let sz : int[] = Array.create N 1

    member t.root i =
        let mutable q = i
        while ( q <> id.[q] ) do
            id.[q] <- id.[id.[q]]
            q <- id.[q]
        q

    member t.find( p, q ) =
        t.root(p) = t.root(q)

    member t.unite( p, q ) =
        let i = t.root( p )
        let j = t.root( q )
        if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
        else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]

let (n,k,l) = readInt3 <| Console.ReadLine()

let rail = UnionFind (n+1)
for i = 1 to k do
    let (p,q) = readInt2 <| Console.ReadLine()
    rail.unite( p, q )
done

let road = UnionFind (n+1)
for i = 1 to l do
    let (p,q) = readInt2 <| Console.ReadLine()
    road.unite( p, q )
done

let cnt_map = new System.Collections.Generic.Dictionary< int * int, int >()
for i = 1 to n do
    let root_tuple = ( (rail.root i), (road.root i) )
    if cnt_map.ContainsKey root_tuple then
        cnt_map.[ root_tuple ] <- cnt_map.[ root_tuple] + 1
    else
        cnt_map.Add( root_tuple, 1 )
done

for i = 1 to n do
    let root_tuple = ( (rail.root i), (road.root i) )
    let cnt = cnt_map.[ root_tuple ]
    Console.Write( cnt )
    if i <> n then
        Console.Write( ' ' )
    else
        Console.Write( '\n' )
done
