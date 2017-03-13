// graph/Graph.fsx
let Dfs (g : graph) (root : vertex) : vertex list * cost array = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let mutable costs = Array.create nodes 0.0
    let stack = new System.Collections.Generic.Stack< vertex * cost >()
    stack.Push (root, 0.0)
    while( stack.Count <> 0 ) do
        let c,cc = stack.Pop()
        if not visited.[c] then
            visited.[c] <- true
            result <- c :: result
            costs.[c] <- cc
            Seq.zip g.[c].Keys g.[c].Values
            |> Seq.map (fun (i,c) -> (i, cc + c))
            |> Seq.iter stack.Push
    done

    List.rev result, costs
