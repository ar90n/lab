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

module String =
    let explode (s:string) =
        [for c in s -> c]

    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

let doit (sx:int) (sy:int) (gx:int) (gy:int) (maze : (int array) array) : int =
    let queue = new System.Collections.Generic.Queue< int * int * int >()
    queue.Enqueue (sy-1,sx-1,0)
    while (queue.Count <> 0) do
        let y,x,c = queue.Dequeue()
        let m = maze.[y].[x]
        if (0 <= m) && (c < m) then
            maze.[y].[x] <- c
            queue.Enqueue (y + 1, x, c + 1 )
            queue.Enqueue (y - 1, x, c + 1 )
            queue.Enqueue (y, x + 1, c + 1 )
            queue.Enqueue (y, x - 1, c + 1 )
    maze.[gy-1].[gx-1]

let (r,c) = Console.ReadLine() |> ParseInt2
let (sy,sx) = Console.ReadLine() |> ParseInt2
let (gy,gx) = Console.ReadLine() |> ParseInt2
ReadLinesN r
|> Seq.map (String.explode >> List.map (fun c -> if c = '.' then System.Int32.MaxValue else -1) >> List.toArray)
|> Seq.toArray
|> doit sx sy gx gy
|> printfn "%d"
