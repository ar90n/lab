import Data.Numbers.Primes
import Data.List

main = do
    let (_,_,a ) = head $ dropWhile (\(_,a,b) -> 0.1 < (a/b))$ drop 1 $ scanl (\(acc,primes,len) b -> let newAcc = (acc + b) in (newAcc,primes + if isPrime newAcc then 1 else 0 ,len +1)) (1,0,1)  $ map ((*2) . (`div`4)) [4..]
    putStrLn (show (2 * a / 4 + 1))
