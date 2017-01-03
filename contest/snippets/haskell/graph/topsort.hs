topSort :: Graph -> [Vertex]
topSort = reverse . postOrd where
    postOrd :: Graph -> [Vertex]
    postOrd g = postorderF (dff g (vertices g)) []
    postorder :: Tree a -> [a] -> [a]
    postorder (Node a ts) = postorderF ts . (a :)
    postorderF   :: Forest a -> [a] -> [a]
    postorderF ts = foldr (.) id $ map postorder ts
