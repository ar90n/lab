powMod :: Int -> Int -> Int -> Int
powMod a b p
    | b == 0 = 1
    | odd b  = ( a * (powMod a (b - 1) p)) `mod` p
    | even b = ( d * d ) `mod` p where
        d = powMod a (b `div` 2) p
