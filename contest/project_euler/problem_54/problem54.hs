import Data.Char
import Data.List

parseCard:: String -> (Int,Char)
parseCard (n:t:[]) | n `elem` ['2'..'9'] = ( ord n - ord '0',t)
                   | n == 'T' = (10,t)
                   | n == 'J' = (11,t)
                   | n == 'Q' = (12,t)
                   | n == 'K' = (13,t)
                   | n == 'A' = (14,t)

calcScore:: [(Int,Char)] -> Int
calcScore cards = let cards'  = sortOn (\a -> fst a) cards
                      nums = map fst cards'
                      pairsList   = filter (\a -> (length a) == 2) $ group nums
                      pairs = (length pairsList, maximum (map head pairsList))
                      triplesList = filter (\a -> (length a) == 3) $ group nums
                      triples = (length triplesList, maximum (map head triplesList) )
                      quadsList   = filter (\a -> (length a) == 4) $ group nums
                      quads = (length quadsList, maximum (map head quadsList))
                      isStraight =  all (\a -> ((fst a) - (snd a)) == 1) $ zip (drop 1 nums) (take 4 nums)
                      isFlash = all (\a -> (snd a) == (snd (head cards'))) cards'
                      isRoyal = all (\a -> 10 <= a) nums
                      maxNum = last nums
                      sndNum = head $ drop 3 nums
                      prop = (pairs, triples, quads, isStraight, isFlash, isRoyal, maxNum, sndNum )
                   in case prop of
                        (     _,      _,      _, True, True, True, maxNum, sndNum ) -> (9 * 1000000 +      maxNum * 10000 + maxNum * 100 + sndNum)
                        (     _,      _,      _, True, True,    _, maxNum, sndNum ) -> (8 * 1000000 +      maxNum * 10000 + maxNum * 100 + sndNum)
                        (     _,      _, (1,qM),    _,    _,    _, maxNum, sndNum ) -> (7 * 1000000 +          qM * 10000 + maxNum * 100 + sndNum)
                        ((1,pM), (1,tM),      _,    _,    _,    _, maxNum, sndNum ) -> (6 * 1000000 + (max pM tM) * 10000 + maxNum * 100 + sndNum)
                        (     _,      _,      _,    _, True,    _, maxNum, sndNum ) -> (5 * 1000000 +      maxNum * 10000 + maxNum * 100 + sndNum)
                        (     _,      _,      _, True,    _,    _, maxNum, sndNum ) -> (4 * 1000000 +      maxNum * 10000 + maxNum * 100 + sndNum)
                        (     _, (1,tM),      _,    _,    _,    _, maxNum, sndNum ) -> (3 * 1000000 +          tM * 10000 + maxNum * 100 + sndNum)
                        ((2,pM),      _,      _,    _,    _,    _, maxNum, sndNum ) -> (2 * 1000000 +          pM * 10000 + maxNum * 100 + sndNum)
                        ((1,pM),      _,      _,    _,    _,    _, maxNum, sndNum ) -> (1 * 1000000 +          pM * 10000 + maxNum * 100 + sndNum)
                        (     _,      _,      _,    _,    _,    _, maxNum, sndNum ) -> (0 * 1000000 +      maxNum * 10000 + maxNum * 100 + sndNum)
                        otherwise                                                   -> 0

match line = let myHand = take 5 line
                 yourHand = drop 5 line
                 myScore = calcScore myHand
                 yourScore = calcScore yourHand
             in if yourScore < myScore then 1 else 0
main = do
    cs <- getContents 
    let ttt = lines cs
        hands = map ( map parseCard . words ) ttt
    putStrLn $ show $ sum $ map match hands
