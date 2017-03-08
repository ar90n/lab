let Diameter (g : graph) : cost =
    Bfs g 0
    |> snd
    |> Seq.mapi (fun i u -> i,u)
    |> Seq.maxBy snd
    |> fst
    |> Bfs g
    |> snd
    |> Array.max
