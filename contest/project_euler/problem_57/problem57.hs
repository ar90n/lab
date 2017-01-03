main = do
    let res = length $ filter (\a -> a) $ take 1000 $ map (\a -> (length ( show (snd a))) < (length ( show ( fst a ) ) ) ) $ scanl (\a b -> ( 2 * (snd a) + (fst a ), (snd a) + (fst a ) )) (3,2) [1..]
    putStr (show res)
