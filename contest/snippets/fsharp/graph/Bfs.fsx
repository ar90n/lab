let Bfs (g : graph) (root : vertex) : vertex list = 
    let nodes = Nodes g
    let visited = Array.create nodes false
    let mutable result = []
    let queue = new System.Collections.Generic.Queue< vertex >()
    queue.Enqueue root
    while( queue.Count <> 0 ) do
        let c = queue.Dequeue()
        if not visited.[c] then
            visited.[c] <- true
            result <- c :: result
            g.[c].Keys |> Seq.iter queue.Enqueue
    done

    List.rev result
