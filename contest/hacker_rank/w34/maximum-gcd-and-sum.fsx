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

module Primes =
    let mutable primes = [7; 5; 3; 2]
    let mutable upper_bound = 7

    let rec create (n : int) : int list =
        for i = upper_bound + 1 to n do
            if isPrime i then
                primes <- i :: primes
            upper_bound <- i
        done
        List.filter ((>=) n) primes

    and isPrime (n : int) : bool =
        n / 2
        |> create
        |> List.forall (fun p -> n % p <> 0)

    let factorize (n : int) : int list =
        let rec doit (n : int) (primes : int list) (factors : int list) : int list =
            match primes with
            | hp::tp when n % hp = 0 -> doit (n / hp) primes (hp::factors)
            | hp::tp -> doit n tp factors
            | [] -> factors
        let primes = create n
        doit n primes []

let rec Gcd a b =
    let (minv, maxv) = (min a b, max a b)
    if minv = 0 then maxv else Gcd (maxv % minv) minv

let primes = Primes.create 1001
let memo0 = Array.create 1000001 -1
let memo1 = Array.create 1000001 -1

let cmd x =
    let queue = new System.Collections.Generic.Queue< int >()
    queue.Enqueue (x)
    while( queue.Count <> 0 ) do
        let x' = queue.Dequeue()
        if memo0.[x'] = -1 then
            memo0.[x'] <- x'
            memo1.[x'] <- x
            primes
            |> List.filter (fun p -> (x' % p) = 0)
            |> List.map (fun p -> x' / p)
            |> List.iter queue.Enqueue
    done

Console.ReadLine()
|> ignore

Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> List.distinct
|> List.sortDescending
|> List.iter (fun x -> cmd x)

for i = 1 to 1000000 do
    if memo0.[i] = i then
        let mutable j = i + i
        while j < 1000000 do
            if memo0.[j] < i then
                memo0.[j] <- i
                memo1.[j] <- memo1.[i]
            j <- j + i
        done
done

Console.ReadLine()
|> ParseIntN
|> Seq.toList
|> List.map (fun x -> (memo0.[x], memo1.[x] + x))
|> List.sortWith (fun (la,lb) (ra,rb) ->  let da = ra - la
                                          let db = rb - lb
                                          match (da,db) with
                                          | (0,_) when 0 < db -> 1
                                          | (0,_) -> -1
                                          | (_,_) when 0 < da -> 1
                                          | _ -> -1 )
|> List.head
|> snd
|> printfn "%d"

//printfn "%A" memo0
//printfn "%A" memo1
