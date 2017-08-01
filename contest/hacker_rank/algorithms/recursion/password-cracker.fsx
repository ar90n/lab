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

let doit (m : string array) (xs : string) =
    let xl = String.length xs
    let ml = Array.length m
    let memo1 = Array.create (xl+1) None
    let memo2 = Array2D.create ml (xl+1) None
    let rec doit' acc (xs : string) mi xl' =
        if memo1.[xl'] <> None then
            memo1.[xl']
        elif memo2.[mi,xl'] <> None then
            memo2.[mi,xl']
        else
            let ms = m.[mi]
            memo2.[mi,xl'] <- match xs with
                              | "" -> memo1.[xl'] <- Some acc
                                      memo1.[xl']
                              | _ when xs.StartsWith ms -> let ls = String.length ms
                                                           let xs' = xs.Substring ls
                                                           [0..(ml-1)]
                                                           |> List.map (fun mi' -> doit' (mi::acc) xs' mi' (xl'-ls))
                                                           |> List.tryPick (function | None -> None
                                                                                     | Some [] -> None
                                                                                     | Some v -> Some v)
                                                           |> function | None -> Some[]
                                                                       | x -> memo1.[xl'] <- x;x
                              | _ -> Some []
            memo2.[mi,xl']
    [0..(ml-1)]
    |> List.map (fun mi -> doit' [] xs mi xl)
    |> List.tryPick (function | None -> None
                              | Some [] -> None
                              | Some v -> Some v)
    |> function | None -> "WRONG PASSWORD"
                | Some [] -> "WRONG PASSWORD"
                | Some v ->  List.rev v
                             |> List.map (fun x -> m.[x])
                             |> List.reduce (fun acc x -> acc + " " + x)

let t = Console.ReadLine() |> ParseInt1
for i = 0 to (t-1) do
    Console.ReadLine() |> ignore
    let m = Console.ReadLine()
            |> (fun x -> x.Split(' '))
    let xs = Console.ReadLine()
    doit m xs |> printfn "%s"
done
