// graph/Graph.fsx
let BellmanFord (g : graph) (root : vertex) : cost list option =
    let nodes = Nodes g
    let result = Array.init nodes ( fun _ -> System.Double.PositiveInfinity)

    let changed = ref false
    result.[root] <- 0.0
    for i = 0 to (nodes - 1) do
        changed := false
        for f = 0 to (nodes - 1) do
            Seq.zip g.[f].Keys g.[f].Values 
            |> Seq.iter (fun (t,c) -> let nc = result.[f] + c
                                      if nc < result.[t] then 
                                        result.[t] <- nc
                                        changed := true )
        done
    done

    if !changed then None else Some (Array.toList result)
