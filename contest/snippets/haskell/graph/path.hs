reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dff g [v]) where
    preorder' :: Tree a -> [a] -> [a]
    preorder' (Node a ts) = (a :) . preorderF' ts
    preorderF' :: Forest a -> [a] -> [a]
    preorderF' ts = foldr (.) id $ map preorder' ts
    preorderF :: Forest a -> [a]
    preorderF ts = preorderF' ts []

path :: Graph -> Vertex -> Vertex -> Bool
path g v w = elem w (reachable g v)
