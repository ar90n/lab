// graph/Graph.fsx
let TopoSort (g : graph) : vertex list option = 
    let nodes = Nodes g
    let status = Array.create nodes 0
    let result = ref []
    let rec visit (v : vertex) = 
        match status.[v] with
        | 0 -> status.[v] <- 1
               g.[v].Keys |> Seq.iter visit
               status.[v] <- 2
               result := v :: !result
        | 1 -> failwith "has cyclic"
        | _ -> ignore()

    try
        seq {0..(nodes-1)} |> Seq.iter (fun v -> if status.[v] = 0 then visit v)
        Some <| List.rev !result
    with
        | Failure "has cyclic" -> None
