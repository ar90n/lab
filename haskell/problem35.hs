primeFactorsAux:: Int -> Int -> [Int]
primeFactorsAux p 1 = []
primeFactorsAux p x = if x `mod` p == 0 then p : (primeFactorsAux p (x `div` p))
                                        else (primeFactorsAux (p+1) x)

primeFactors:: Int -> [Int]
primeFactors x = primeFactorsAux 2 x
