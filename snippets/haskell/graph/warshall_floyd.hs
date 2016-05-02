warshall_floyd :: Graph -> Maybe(Array (Vertex, Vertex) Cost)
warshall_floyd g = checkResult $ runSTArray $ do
    costs <- newArray ((l,l),(h,h)) (div maxBound 2) :: ST s (STArray s (Vertex, Vertex) Cost)
    mapM_ (initCost costs) $ [((v,v), 0) | v<-vertices g] ++ [((u,v), c) | (u,v,c)<-edges g]
    return costs
    doit costs (vertices g)
    where
        (l,h) = bounds g
        checkResult :: Array (Vertex,Vertex) Cost -> Maybe(Array (Vertex, Vertex) Cost)
        checkResult costs = let isValid = all (==0) $ [ costs ! (i,i) | i <- [l..h] ]
                            in if isValid then Just costs else Nothing
        initCost :: STArray s (Vertex, Vertex) Cost -> ((Vertex,Vertex), Cost) -> ST s ()
        initCost costs ((f,t),c) = do
            c' <- readArray costs (f,t)
            writeArray costs (f,t) (min c c')
        updateCost :: STArray s (Vertex, Vertex) Cost -> Vertex -> (Vertex,Vertex)-> ST s ()
        updateCost costs k (f,t) = do
            c <- readArray costs (f,t)
            c' <- (+) <$> readArray costs (f,k) <*> readArray costs (k,t)
            writeArray costs (f,t) (min c c')
        doit :: STArray s (Vertex, Vertex) Cost -> [Vertex] -> ST s (STArray s (Vertex, Vertex) Cost)
        doit costs [] = return costs
        doit costs (v:vs) = do
            mapM_ (updateCost costs v) $ [(i,j) | i <- [l..h], j <- [l..h]]
            doit costs vs
