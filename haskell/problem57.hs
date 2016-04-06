data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

aux (Branch x llf lrf) (Branch y rlf rrf) = (aux llf rrf) && (aux lrf rlf)
aux Empty Empty = True
aux _ _ = False

symmetric Empty = True
symmetric (Branch _ lf rf) =  aux lf rf


insert x Empty = Branch x Empty Empty
insert x (Branch y lf rf) | x == y    = Branch y lf rf
                          | x < y     = Branch y (insert x lf ) rf
                          | otherwise = Branch y lf (insert x rf )

constructAux::Ord a => [a] -> Tree a
constructAux [] = Empty
constructAux (x:xs) = insert x (constructAux xs)

construct::Ord a => [a] -> Tree a
construct  = constructAux . reverse
