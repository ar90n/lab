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

let integral (memo : (int[])[,]) i j v =
    let add k l =
        [0..9] |> List.iter (fun x -> memo.[i,j].[x] <- memo.[i,j].[x] + memo.[k,l].[x])
    let sub k l =
        [0..9] |> List.iter (fun x -> memo.[i,j].[x] <- memo.[i,j].[x] - memo.[k,l].[x])
    add (i-1) j
    add i (j-1)
    sub (i-1) (j-1)
    memo.[i,j].[v] <- memo.[i,j].[v] + 1

let doit (memo : (int[])[,]) i j k l =
    let tmp = Array.zeroCreate 10
    for m = 0 to 9 do
        tmp.[m] <- memo.[i,j].[m] - memo.[k,j].[m] - memo.[i,l].[m] + memo.[k,l].[m]
    done
    tmp
    |> Array.filter (fun e -> e % 2 = 1)
    |> Array.length
    |> (fun c -> c < 2)
    

let n,m = Console.ReadLine() |> ParseInt2
let table = Array2D.create n m 0
ReadLinesN n
|> Seq.map ParseIntN
|> Seq.iteri (fun i xs -> Seq.iteri (fun j x -> table.[i,j] <- x) xs)

let memo = Array2D.init (n+1) (m+1) (fun _ _ -> Array.zeroCreate 10)
table
|> Array2D.iteri (fun i j v -> integral memo (i+1) (j+1) v)

let check (i0,j0,k0,l0) (i1,j1,k1,l1) = 
    ((k0 - i0) * (l0 - j0)) < ((k1 - i1) * (l1 - j1))

let res = ref (0,0,0,0)
for i = 0 to (n-1) do
    for j = 0 to (m-1) do
        for k = (i+1) to n do
            for l = (j+1) to m do
                if (doit memo i j k l) && check !res (i,j,k,l) then  res := (i,j,k,l) else ()
            done
        done;
    done
done;
let (a,b,c,d) = !res
let c' = c - 1
let d' = d - 1
let area = (c - a) * (d - b)
printfn "%d" area
printfn "%d %d %d %d" a b c' d'
