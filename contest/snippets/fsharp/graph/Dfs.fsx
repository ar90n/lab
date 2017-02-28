let Dfs (g : graph) (root : vertex) : vertex list = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let stack = new System.Collections.Generic.Stack< vertex >()
    stack.Push root
    while( stack.Count <> 0 ) do
        let c = stack.Pop()
        if not visited.[c] then
            visited.[c] <- true
            result <- c :: result
            g.[c].Keys |> Seq.iter stack.Push
    done

    List.rev result
