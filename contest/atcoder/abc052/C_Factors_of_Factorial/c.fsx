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

let n = ParseInt1 <| Console.ReadLine()
let primes = Primes.create n
let counts = new System.Collections.Generic.Dictionary< int, int>()
primes
|> List.iter (fun p -> counts.[p] <- 1)
counts.[1] <- 1

[2 .. n]
|> List.map Primes.factorize
|> List.iter (fun ps -> List.iter (fun p -> counts.[p] <- counts.[p] + 1 ) ps )

counts.Values
|> Seq.fold (fun acc c -> (acc * int64(c)) % 1000000007L) 1L
|> Console.WriteLine
