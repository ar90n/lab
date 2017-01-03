import qualified Data.Map.Strict as M
import Data.List
import Data.List.Split
import Data.Char
import Data.Bits


createHist:: (M.Map Char Int) -> Char -> (M.Map Char Int)
createHist hist key = M.insertWith (+) key 1 hist

--calcScore::  (M.Map Char Int) -> [Float]
calcScore hist = let refRatio = [('e',11.40962588),
                                 ('a',8.446499792),
                                 ('t',8.184687661),
                                 ('i',7.130098608),
                                 ('o',7.051799653),
                                 ('s',6.973500697),
                                 ('n',6.770412782),
                                 ('r',6.222320096),
                                 ('h',4.255058847),
                                 ('l',3.87335144 ),
                                 ('d',3.870904598),
                                 ('c',3.195576109),
                                 ('u',2.953338716),
                                 ('m',2.671951846),
                                 ('p',2.023538623),
                                 ('f',1.984389146),
                                 ('g',1.820450708),
                                 ('y',1.793535443),
                                 ('w',1.698108591),
                                 ('b',1.644278059),
                                 ('v',0.998311679),
                                 ('k',0.8955443  ),
                                 ('j',0.205534757),
                                 ('x',0.176172649),
                                 ('q',0.08319264 ),
                                 ('z',0.078298955)]
                     ref = [ x | (_,x) <- sortOn fst refRatio]
                     tmp = sortOn fst $ filter (\(a,_) -> a `elem` ['a'..'z'] ) $ M.toList hist
                     acc = foldl (\a (_,b) -> a + b) 0 tmp
                     vec = [ 100 *  fromIntegral(x) / fromIntegral(acc) | (_,x) <- tmp]
                 in sum [x * y | (x,y) <- zip ref vec]


findKey:: [Int] -> [(Int,Int,Int)] -> Int
findKey codes keys = let emp = M.fromList [ (x,0) | x <- ['a'..'z'] ]
                         cands = [ [chr $ xor c k  |(c,k) <- zip codes $ cycle [k0,k1,k2]] | (k0,k1,k2) <- keys ]
                         hists = [ foldl (\a b -> createHist a b) emp cand  | cand <- cands ]
                         scores = [ calcScore hist | hist <- hists ]
                         (ek0,ek1,ek2) = fst . head . take 1 . sortBy (\(_,x) (_,y) -> compare y x ) $ zip keys scores
                     in  sum [xor c k |(c,k) <- zip codes $ cycle [ek0,ek1,ek2]]

main = do
    line <- getLine
    let codes = [ read x::Int | x <- splitOn "," line ]
        keys = [ (ord a, ord b, ord c) | a <- ['a'..'z'], b <- ['a'..'z'] ,c <- ['a'..'z']]
        res = findKey codes keys
    putStrLn (show res)
