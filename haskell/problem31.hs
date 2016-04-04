--isPrime:: Int -> Bool
isPrime 1 = False
isPrime x = all (\a -> x `mod` a /= 0 ) $ takeWhile (\a -> a * a <= x ) [2..]
