isPrime:: Int -> Bool
isPrime 1 = False
isPrime x = all (\a -> x `mod` a /= 0 ) $ takeWhile (\a -> a * a <= x ) [2..]

primesR:: Int -> Int  -> [Int]
primesR b e = filter isPrime [b..e]

goldbach:: Int -> (Int, Int)
goldbach x =  head [(x-y,y) | y <- (primesR 2 (x `div` 2 + 1)),isPrime (x - y)]
