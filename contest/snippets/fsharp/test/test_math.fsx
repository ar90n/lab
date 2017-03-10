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

let ReadLines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

//--------------------------------------------------------------------------------

let rec Gcd a b =
    let (minv, maxv) = (min a b, max a b)
    if minv = 0 then maxv else Gcd (maxv % minv) minv

let Lcm a b =
    a * b / (Gcd a b)

let rec PowMod (a: int64) (b : int64) (c : int64) =
    if b = 0L then 1L
    else if ( b % 2L ) = 1L then ( a * ( PowMod a (b - 1L) c ) ) % c
    else 
        let d = PowMod a (b / 2L) c
        ( d * d ) % c

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

//--------------------------------------------------------------------------------
Console.WriteLine "OK"