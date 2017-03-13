// graph/Graph.fsx
// collection/PriorityQueue.fsx
let Dijkstra (g : graph) (root : vertex) : (graph * cost array) =
    let nodes = Nodes g
    let costs = Array.create nodes System.Double.PositiveInfinity

    let comp (_,a) (_,b) = 
        match a,b with
        | _ when a < b -> -1
        | _ when a > b ->  1
        | _ -> 0
    let pqueue = Edges g |> List.fold (fun q (f,t,c) -> PriorityQueue.push q ((f,t,c),c)) (PriorityQueue.init comp)
    costs.[root] <- 0.0
    let rec doit (costs : cost array) (edges : edge list) (pqueue : (edge * cost) PriorityQueue.pqueue) : cost array * edge list =
        if PriorityQueue.isEmpty pqueue then
            costs, edges
        else
            let (((_,t,c) as e,ac),pqueue') = PriorityQueue.pop pqueue
            if ac < costs.[t] then
                let edges' = e :: edges
                costs.[t] <- ac
                Seq.zip g.[t].Keys g.[t].Values
                |> Seq.map (fun (t',c') -> (t,t',c'))
                |> Seq.fold ( fun q (f,t,c) -> PriorityQueue.push q ((f,t,c), ac + c) ) pqueue'
                |> doit costs edges'
            else
                doit costs edges pqueue'
    let (rc,re ) = doit costs [] pqueue
    DGraph nodes re, rc
