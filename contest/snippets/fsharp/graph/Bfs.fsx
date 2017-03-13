// graph/Graph.fsx
let Bfs (g : graph) (root : vertex) : vertex list * cost array = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let mutable costs = Array.create nodes 0.0
    let queue = new System.Collections.Generic.Queue< vertex * cost >()
    queue.Enqueue (root, 0.0)
    while( queue.Count <> 0 ) do
        let c,cc = queue.Dequeue()
        if not visited.[c] then
            visited.[c] <- true
            result <- c :: result
            costs.[c] <- cc
            Seq.zip g.[c].Keys g.[c].Values
            |> Seq.map (fun (i,c) -> (i, cc + c))
            |> Seq.iter queue.Enqueue
    done

    List.rev result, costs
