bfs :: Graph -> [Vertex] -> [Vertex]
bfs g vs = runST $ do
    visited <- newArray (bounds g) False :: (ST s (STArray s Vertex Bool ))
    foldM (\a v -> (a ++) <$> doit visited (S.singleton v) ) [] vs where
        doit :: STArray s Vertex Bool -> S.Seq Vertex -> ST s ([Vertex])
        doit visited candidates = case S.viewl candidates of
            S.EmptyL -> return []
            c :< cs  -> do
                isVisited <- readArray visited c
                if isVisited then do
                    ncs <- doit visited cs
                    return ncs
                else do
                    writeArray visited c True
                    let candidates' = cs >< (S.fromList $ map fst (g!c))
                    ncs <- doit visited candidates'
                    return (c : ncs)
