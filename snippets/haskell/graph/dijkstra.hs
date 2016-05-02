dijkstra :: Vertex -> Graph -> Maybe (Array Vertex Cost)
dijkstra s g = Just $ runSTArray $ do
    costs <- newArray (bounds g) maxBound :: (ST s (STArray s Vertex Cost))
    let pqueue = singleton (\(v0,c0) (v1,c1) -> c0 < c1) (s,0)
    doit costs pqueue where
    doit :: STArray s Vertex Cost -> PriorityQueue (Vertex,Cost) -> ST s (STArray s Vertex Cost)
    doit costs queue | isEmpty queue = return costs
                     | otherwise     = do
                            let ((v,c),queue') = pop queue
                            c' <- readArray costs v
                            if (c < c') then do
                                writeArray costs v c
                                let queue'' = foldl (\q (v,cc) -> push q (v,c+cc)) queue (g!v)
                                doit costs queue''
                            else do
                                doit costs queue'
