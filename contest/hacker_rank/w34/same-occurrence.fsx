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


let n,q = Console.ReadLine() |> ParseInt2
let ns = Console.ReadLine() |> ParseIntN |> Seq.toList

let m = System.Collections.Generic.Dictionary<int,int>()
ns |> List.iter (fun x -> if not (m.ContainsKey x) then m.Add(x,m.Count) else ())
let ns' = List.map (fun x -> m.[x]) ns

let cnv x =
    if m.ContainsKey x then
        m.[x]
    else
        m.Count

let doit a b =
    let accum st x =
        if x <> a && x <> b then
            let result = fst st
            let sublists = snd st
            let ww = List.head sublists |> fst
            let sublists' = (ww + 1,[]) :: (List.tail sublists)
            let result' = result + ww
            (result', sublists')
        else
            let result = fst st
            let sublists = snd st
            let w',sublists' = sublists
                                   |> List.map (fun (w,ls) -> let ls' = match ls with
                                                                        | h::t when  h <> x -> t
                                                                        | _ -> x::ls
                                                              (w,ls'))
                                   |> List.partition (fun (_,ls) -> ls = [])
                                   |> (fun (ys,xs) -> (List.fold (fun acc (a,_) -> acc + a) 0 ys, xs))
            let sublists'' = (1 + w',[]) :: sublists'
            let result' = result + w'
            (result', sublists'')

    List.fold (fun st x -> accum st x) (0,[(1,[])]) ns'

ReadLinesN q
|> Seq.toList
|> List.map (ParseInt2 >> fun (a,b) -> (cnv a, cnv b))
|> List.map (fun (a,b) -> doit a b)
|> List.map fst
|> List.iter (printfn "%A")
