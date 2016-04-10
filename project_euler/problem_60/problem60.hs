import Data.Numbers.Primes

isPrimePair:: Int -> Int -> Bool
isPrimePair x y = isPrime $ (read ( (show x) ++ (show y))::Int)

aux xs = not $ foldr (\a b -> a && b) True [isPrimePair x y | x <- xs, y <- xs, x /= y ]

main = do 
    let res = head . dropWhile aux $[[a,b,c,d,e] | a <- primes, b <- takeWhile (\x -> x < a) primes, c <- takeWhile(\x -> x < b) primes, d <- takeWhile(\x -> x < c) primes, e <- takeWhile(\x -> x < d) primes]
    putStrLn ( show res )
