import Data.List

huffman h = sortOn (\a -> fst a) $ snd $ head $ aux [ (snd x,[(fst x,"")]) | x <- sortOn (\a -> snd a) h]
            where aux [c1] = [c1]
                  aux (c1:c2:h) = let new_freq = (fst c1) + (fst c2)
                                      new_c1 = [ (fst x, ('0' : (snd x)) ) | x <- (snd c1) ]
                                      new_c2 = [ (fst x, ('1' : (snd x)) ) | x <- (snd c2) ]
                                      c12 = ( new_freq, new_c1 ++ new_c2 )
                                  in aux $ sortOn (\a -> fst a ) (c12:h)
