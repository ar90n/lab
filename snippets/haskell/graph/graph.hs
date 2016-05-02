type Edge = (Vertex, Vertex, Cost)
type Cost = Int
type Vertex = Int

type Graph = Table [(Vertex,Cost)]
type Table a = Array Vertex a
type Bounds = (Vertex, Vertex)

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [(u,v,c) | u <- vertices g, (v,c) <- g!u]

costs :: Graph -> [Cost]
costs g = [c | u <- vertices g, (_,c) <- g!u]
dists = costs
weighs = costs

cost :: Edge -> Cost
cost (_,_,c) = c
dist = cost
weight = cost

mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]

buildG :: Bounds -> [Edge] -> Graph
buildG bs = accumArray (flip (:)) [] bs . map out where
    out (u,v,c) = (u,(v,c))

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [(v,u,c) | (u,v,c) <- edges g]

outdegree :: Graph -> Table Int
outdegree = mapT (const length)

indegree :: Graph -> Table Int
indegree = outdegree . transposeG
