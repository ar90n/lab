bellman_ford :: Vertex -> Graph -> Maybe (Array Vertex Cost)
bellman_ford s g = if ak==go ak then Just ak else Nothing where
    k = uncurry subtract $ bounds g
    ak = (!! (k-1)) $ iterate go a0 -- TBD: early stopping 
    c0 = div maxBound 2 :: Int
    a0 = array (bounds g) ((s,0):[(v,c0) | v<-vertices g, v/=s])
    gt = transposeG g
    go :: Array Vertex Cost -> Array Vertex Cost
    go a = a // [(v, f v) | v<-vertices g] where
        f v = minimum $ a!v : [a!w+c | (w,c)<-gt!v]
