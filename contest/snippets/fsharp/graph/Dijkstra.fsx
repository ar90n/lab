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
    let pqueue = Seq.zip g.[root].Keys g.[root].Values
                 |> Seq.map (fun (t',c') -> (root,t',c'))
                 |> Seq.fold (fun q (f,t,c) -> PriorityQueue.push q ((f,t,c),c)) (PriorityQueue.init comp)
    costs.[root] <- 0.0
    let rec doit (costs : cost array) (edges : edge list) (pqueue : (edge * cost) PriorityQueue.pqueue) : cost array * edge list =
        if PriorityQueue.isEmpty pqueue then
            costs, edges
        else
            let (((_,t,c) as e,ac),pqueue') = PriorityQueue.pop pqueue
            let q'' = if ac < costs.[t,b] then
                        costs.[t,b] <- ac
                        Seq.zip g.[t].Keys g.[t].Values
                        |> Seq.filter (fun (t',c') -> (ac + c') < costs.[t',b ||| fs.[t']] )
                        |> Seq.fold (fun q (t',c') -> PriorityQueue.push q ((t,t',c'),b ||| fs.[t'], ac + c') ) pqueue'
                      else
                        pqueue'
            doit costs q''
    let (rc,re ) = doit costs [] pqueue
    DGraph nodes re, rc
