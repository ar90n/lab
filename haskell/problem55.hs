data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree:: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let ch = n - 1
                 ch1 = ch `div` 2
                 ch2 = ch - ch1
             in  if ch1 == ch2 then [ Branch 'x' lf rf | lf <- cbalTree ch1 , rf <- cbalTree ch2 ]
                               else [ Branch 'x' lf rf | lf <- cbalTree ch1 , rf <- cbalTree ch2 ] ++ [ Branch 'x' lf rf | lf <- cbalTree ch2 , rf <- cbalTree ch1 ] 
