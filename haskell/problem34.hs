totient:: Int -> Int
totient x = length $ filter (\a -> gcd a x == 1) [1..x]
