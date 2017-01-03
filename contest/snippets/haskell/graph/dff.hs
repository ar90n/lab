dff :: Graph -> [Vertex] -> Forest Vertex
dff g vs = prune (bounds g) (map (generate g) vs) where
    generate :: Graph -> Vertex -> Tree Vertex
    generate g v = Node v (map (generate g . fst) (g!v))
    prune :: Bounds -> Forest Vertex -> Forest Vertex
    prune bnds ts = runST $ do
        visited <- (newArray bnds False)::(ST s (STArray s Vertex Bool))
        chop visited ts
    chop :: STArray s Int Bool -> Forest Vertex -> ST s (Forest Vertex)
    chop _ [] = return []
    chop visited (Node v ts : us) = do
        isVisited <- readArray visited v
        if isVisited then
            chop visited us
        else do
            writeArray visited v True
            as <- chop visited ts
            bs <- chop visited us
            return (Node v as : bs)
