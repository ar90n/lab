data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

hbalTree c 0 = [ Branch c Empty Empty, Empty ]
hbalTree c n = [ Branch c lh rh | lh <- hbalTree c (n-1), rh <- hbalTree c (n-1) ]
