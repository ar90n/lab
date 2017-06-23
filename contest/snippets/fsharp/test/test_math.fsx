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

type vector2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                   and ^a : (static member (-) : ^a * ^a -> ^a)
                   and ^a : (static member (*) : ^a * ^a -> ^a)
                   and ^a : (static member (/) : ^a * ^a -> ^a)
                   and ^a : (static member (~-) : ^a -> ^a)
                   and ^a : (static member Abs : ^a -> ^a)
                   and ^a : (static member op_Explicit : ^a -> float)
                   and ^a : comparison> =
    {
        x : ^a
        y : ^a
    }

    static member inline (+) (v0, v1) = { x = v0.x + v1.x; y = v0.y + v1.y }
    static member inline (-) (v0, v1) = { x = v0.x - v1.x; y = v0.y - v1.y }
    static member inline (*) (v, k) = { x = k * v.x; y = k * v.y }
    static member inline (*) (k, v) = { x = k * v.x; y = k * v.y }
    static member inline (/) (v, k) = { x = v.x / k; y = v.y / k }
    static member inline (~-) (v) = { x = -v.x; y = -v.y }
    static member inline compare v0 v1 = if compare v0.x v1.x <> 0 then compare v0.x v1.x else compare v0.y v1.y

module Vector2d =
    let inline dot (v0 : ^a vector2d) (v1 : ^a vector2d) : ^a = v0.x * v1.x + v0.y * v1.y
    let inline cross (v0 : ^a vector2d) (v1 : ^a vector2d) : ^a = v0.x * v1.y - v0.y * v1.x
    let inline normL1 (v : ^a vector2d) : ^a = (abs v.x) + (abs v.y)
    let inline normL2 (v : ^a vector2d) : float = dot v v |> float |> sqrt
    let inline isOrthogonal (v0 : ^a vector2d) (v1 : ^a vector2d) : bool = dot v0 v1 |> ((=) Core.LanguagePrimitives.GenericZero)
    let inline isParallel (v0 : ^a vector2d) (v1 : ^a vector2d) : bool = cross v0 v1 |> ((=) Core.LanguagePrimitives.GenericZero)
    let inline project (v0 : ^a vector2d) (v1 : ^a vector2d) : float vector2d = (float (dot v0 v1)) / (normL2 v0) * ({x = float v0.x; y = float v0.y})
    let inline reflect (v0 : ^a vector2d) (v1 : ^a vector2d) : float vector2d = (project v0 v1) + (project v0 v1) - ({x = float v1.x; y = float v1.y})
    let inline toFloat (v : ^a vector2d) : float vector2d = {x = float v.x; y = float v.y}

type relation2d =
    | COUNTER_CLOCKWISE
    | CLOCKWISE
    | ONLINE_BACK
    | ONLINE_FRONT
type point2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                  and ^a : (static member (-) : ^a * ^a -> ^a)
                  and ^a : (static member (*) : ^a * ^a -> ^a)
                  and ^a : (static member (/) : ^a * ^a -> ^a)
                  and ^a : (static member (~-) : ^a -> ^a)
                  and ^a : (static member Abs : ^a -> ^a)
                  and ^a : (static member op_Explicit : ^a -> float)
                  and ^a : comparison> = ^a vector2d
module Point2d =
    open Vector2d
    let eps = 1e-12
    let inline direction (p0 : ^a point2d) (p1 : ^a point2d) : ^a vector2d = { x= p1.x - p0.x; y = p1.y - p0.y }
    let inline distanceL1 v0 v1 = direction v0 v1 |> normL1
    let inline distanceL2 v0 v1 = direction v0 v1 |> normL2
    let inline relation (p0 : ^a point2d) (p1 : ^a point2d) (p2 : ^a point2d) : relation2d =
        let d1 = direction p0 p1
        let d2 = direction p0 p2
        let cross_value = cross d1 d2 |> float
        let dot_value = dot d1 d2 |> float
        match true with
        | _ when eps < cross_value -> COUNTER_CLOCKWISE
        | _ when cross_value < -eps -> CLOCKWISE
        | _ when dot_value < -eps -> ONLINE_BACK
        | _ -> ONLINE_FRONT

type line2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                 and ^a : (static member (-) : ^a * ^a -> ^a)
                 and ^a : (static member (*) : ^a * ^a -> ^a)
                 and ^a : (static member (/) : ^a * ^a -> ^a)
                 and ^a : (static member (~-) : ^a -> ^a)
                 and ^a : (static member Abs : ^a -> ^a)
                 and ^a : (static member op_Explicit : ^a -> float)
                 and ^a : comparison> =  ^a point2d * ^a vector2d
module Line2d =
    open Vector2d
    open Point2d
    let inline distanceFromPointL2 (line : ^a line2d) (point  : ^a point2d) : float =
        let (p, v0) = line
        direction p point
        |> cross v0
        |> (fun s -> (float s) / (normL2 v0))
        |> abs


let PI : float = 2.0 * asin(1.0)

type ContainStatus =
    | OUT
    | ON
    | IN
let inline PolygonContains (polygon : (^a point2d) list) (point : ^a point2d) : ContainStatus =
    let eps = 1e-12
    let h = List.head polygon
    let doit (p0 : 'a point2d) (p1 : 'a point2d) =
        let a = if p0.y < p1.y then p0 else p1
        let b = if p0.y < p1.y then p1 else p0
        if ((((float >> abs) (Vector2d.cross a b)) < eps) && ((float (Vector2d.dot a b)) < eps)) then (1,0)
        else if ( ((float a.y) < eps) && (eps < (float b.y)) && (eps < (float (Vector2d.cross a b)))) then (0,1)
        else (0,0)

    List.rev polygon
    |> List.fold (fun (l,p0) p1 -> ((p0,p1)::l,p1)) ([],h)
    |> fst
    |> List.fold (fun (oc,cc) (p0,p1) -> let (no,nc) = doit p0 p1
                                         (oc + no, cc + nc)) (0,0)
    |> (fun (oc,cc) -> match (oc,cc) with
                       | _ when 0 < oc -> ON
                       | _ when 0 < cc -> IN
                       | _ -> OUT)

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

type segment2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                    and ^a : (static member (-) : ^a * ^a -> ^a)
                    and ^a : (static member (*) : ^a * ^a -> ^a)
                    and ^a : (static member (/) : ^a * ^a -> ^a)
                    and ^a : (static member (~-) : ^a -> ^a)
                    and ^a : (static member Abs : ^a -> ^a)
                    and ^a : (static member op_Explicit : ^a -> float)
                    and ^a : comparison> = ^a point2d * ^a point2d
module Segment2d =
    open Vector2d
    open Point2d

    let inline isIntersect (seg0 : ^a segment2d) (seg1 : ^a segment2d) : bool =
        let (p0,p1) = seg0
        let (p2,p3) = seg1
        let check (p0 : ^a vector2d) (p1 : ^a vector2d) (p2 : ^a vector2d) (p3 : ^a vector2d) : bool =
            match (relation p0 p1 p2, relation p0 p1 p3) with
            | (COUNTER_CLOCKWISE, CLOCKWISE)
            | (CLOCKWISE, COUNTER_CLOCKWISE)
            | (ONLINE_FRONT, _)
            | (_, ONLINE_FRONT) -> true
            | _ -> false
        (check p0 p1 p2 p3) && (check p2 p3 p0 p1)

    let inline distanceFromPointL2 (seg : ^a segment2d) (point2d : ^a point2d) : float =
        let (p0,p1) = seg
        let v0 = direction p0 p1
        let v1 = direction p0 point2d
        let v2 = direction p1 point2d
        match true with
        | _ when dot v0 v1 < Core.LanguagePrimitives.GenericZero -> distanceL2 p0 point2d
        | _ when dot (-v0) v2 < Core.LanguagePrimitives.GenericZero -> distanceL2 p0 point2d
        | _ -> cross v0 v1 |> (fun s -> (float s) / (normL2 v0)) |> abs

    let inline distanceL2 (seg0 : ^a segment2d) (seg1 : ^a segment2d) : float =
        let (p0,p1) = seg0
        let (p2,p3) = seg1
        if isIntersect seg0 seg1 then 0.0
        else min (min (distanceFromPointL2 seg0 p2) (distanceFromPointL2 seg0 p3)) (min (distanceFromPointL2 seg1 p0) (distanceFromPointL2 seg1 p1))

    let inline intersecxtPoint (seg0 : ^a segment2d) (seg1 : ^a segment2d) : (float point2d) option =
        if not (isIntersect seg0 seg1) then None
        else
            let (p0,p1) = seg0
            let (p2,p3) = seg1
            let v0 = direction p0 p1
            let v1 = direction p2 p3 |> toFloat
            let d0 = cross v0 <| direction p0  p2 |> abs |> float
            let d1 = cross v0 <| direction p0  p3 |> abs |> float
            let t = d0 / ( d0 + d1 )
            Some ((toFloat p2) + v1 * t)

//--------------------------------------------------------------------------------
Console.WriteLine "OK"