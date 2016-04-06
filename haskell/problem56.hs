data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

aux (Branch x llf lrf) (Branch y rlf rrf) = (aux llf rrf) && (aux lrf rlf)
aux Empty Empty = True
aux _ _ = False

symmetric Empty = True
symmetric (Branch _ lf rf) =  aux lf rf
