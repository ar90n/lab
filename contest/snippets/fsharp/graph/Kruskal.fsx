// graph/Graph.fsx
// graph/UnionFind.fsx
let Kruskal (g : graph) (root : vertex) : graph =
    let uf = Nodes g |> UnionFind.create
    Edges g
    |> List.sortBy (fun (_,_,c) -> c)
    |> List.fold (fun acc (a,b,c) -> if UnionFind.connected uf a b then acc else UnionFind.unite uf a b; (a,b,c)::acc) []
    |> DGraph (Nodes g)
