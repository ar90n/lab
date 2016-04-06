data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

aux (Branch x llf lrf) (Branch y rlf rrf) = (aux llf rrf) && (aux lrf rlf)
aux Empty Empty = True
aux _ _ = False

symmetric Empty = True
symmetric (Branch _ lf rf) =  aux lf rf


cbalTree:: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let ch = n - 1
                 ch1 = ch `div` 2
                 ch2 = ch - ch1
             in  if ch1 == ch2 then [ Branch 'x' lf rf | lf <- cbalTree ch1 , rf <- cbalTree ch2 ]
                               else [ Branch 'x' lf rf | lf <- cbalTree ch1 , rf <- cbalTree ch2 ] ++ [ Branch 'x' lf rf | lf <- cbalTree ch2 , rf <- cbalTree ch1 ] 

symCbalTree:: Int -> [Tree Char]
symCbalTree n = filter symmetric $ cbalTree n
