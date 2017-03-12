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

let n = Console.ReadLine() |> ParseInt1
let a = Console.ReadLine() |> ParseIntN |> Array.toList

let split a =
    let split2 l =
        let rec split2' res hs l =
            match l with
            | h::t -> split2' ((h::hs)::res) (h::hs) t
            | [] -> res
        split2' [] [] l

    let rec split' res hs ts = 
        match ts with
        | ht :: tt -> split' (((split2 (ht::hs)) @ (List.rev (List.map List.rev <| split2 ts)))::res) (ht::hs) tt
        | [] -> res
    split' [] [] a

let calc a =
    let n = List.length a
    List.foldBack (fun x (l,r) -> x::r, l) a ([], [])
    |> (fun (e,o) -> (List.fold (+) 0 e, List.fold (+) 0 o))

split a
|> List.map (List.filter (fun l -> 1 < List.length l))
|> List.map (fun l -> List.map calc l)
|> List.map (List.sortWith (fun (_,a) (_,b) -> compare b a))
|> List.map (List.head >> fst)
|> List.max
|> printfn "%A"
