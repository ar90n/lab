let Dijkstra (g : graph) (root : vertex) : cost list =
    let nodes = Nodes g
    let costs = Array.create nodes System.Double.PositiveInfinity
    let comp (_,a) (_,b) = 
        match a,b with
        | _ when a < b -> -1
        | _ when a > b ->  1
        | _ -> 0
    let pqueue = ref <| PriorityQueue comp

    pqueue := Push !pqueue (root, 0.0)
    while( not (IsEmpty !pqueue) ) do
        let ((v,c),npqueue) = Pop !pqueue
        pqueue := npqueue
        if  System.Double.IsInfinity costs.[v] then
            costs.[v] <- c
            Seq.zip g.[v].Keys g.[v].Values |> Seq.iter ( fun (nv,ec) -> pqueue := (Push !pqueue (nv,c + ec)) )
    done

    Array.toList costs
