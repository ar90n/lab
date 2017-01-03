import Data.List

primeFactorsMult:: Int -> [(Int,Int)]
primeFactorsMult xs = map (\a -> (head a, length a)) . group $ aux 2 xs
                      where aux p 1 = []
                            aux p x = if x `mod` p == 0 then p : (aux p (x `div` p))
                                                        else (aux (p+1) x)

totient:: Int -> Int
totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult m]
