// graph/Graph.fsx
type UnionFind = int[] * int[]

let Root ( uf : UnionFind ) ( i : int ) =
    let (id,sz) = uf
    let mutable q = i
    while ( q <> id.[q] ) do
        id.[q] <- id.[id.[q]]
        q <- id.[q]
    q

let Find ( uf : UnionFind ) ( p : int ) ( q : int ) =
    (Root uf p) = (Root uf q)

let Unite ( uf : UnionFind ) ( p : int ) ( q : int ) =
    let (id,sz) = uf
    let i = Root uf p
    let j = Root uf q
    if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
    else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]
