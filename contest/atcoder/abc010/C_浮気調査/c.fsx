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

open Vector2d
let vs = Console.ReadLine() |> ParseIntN
let s = { x = vs.[0]; y = vs.[1] }
let d = { x = vs.[2]; y = vs.[3] }
let tv = vs.[4] * vs.[5] |> float
Console.ReadLine()
|> ParseInt1
|> ReadLinesN
|> Seq.map (ParseInt2 >> (fun (x,y) -> {x = x; y = y}))
|> Seq.map (fun m -> (normL2 (m - s)) + (normL2 (d - m)))
|> Seq.filter (fun l -> l <= tv)
|> Seq.length
|> function
    | 0 -> "NO"
    | _ -> "YES"
|> Console.WriteLine
