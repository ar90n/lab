// graph/Graph.fsx
// collection/PriorityQueue.fsx
let Prim (g : graph) (root : vertex) : graph =
    let nodes = Nodes g
    let visited = Array.create nodes false
    let comp (_,_,a) (_,_,b) = 
        match a,b with
        | _ when a < b -> -1
        | _ when a > b ->  1
        | _ -> 0
    let pqueue = ref <| PriorityQueue comp
    Seq.zip g.[root].Keys g.[root].Values |> Seq.iter (fun (t,c) -> pqueue := Push !pqueue (root,t,c) )

    visited.[root] <- true
    let mutable edges = []
    while( not (IsEmpty !pqueue) ) do
        let (e,npqueue) = Pop !pqueue
        let (f,t,c) = e
        pqueue := npqueue

        if not visited.[t] then
            visited.[t] <- true
            edges <- e :: edges
            Seq.zip g.[t].Keys g.[t].Values |> Seq.iter (fun (tt,c) -> pqueue := Push !pqueue (t,tt,c) )
    done
    DGraph nodes edges