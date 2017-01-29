import System.Random

--rnd_select:: [a] -> Int -> IO [a]
--rnd_select s n = do
--    gen <- newStdGen
--    return $ take n [ s !! x | x <- randomRs (0, length(s) - 1 ) gen ]

rnd_select_aux :: RandomGen g => [a] -> Int -> g -> ([a],g)
rnd_select_aux s 0 g = ([],g)
rnd_select_aux [] _ g = ([],g)
rnd_select_aux s n g
    | n == (length s) = (s,g)
    | otherwise = let (i,g') = randomR (0,length s - 1) g
                      (ys,zs) = splitAt i s
                      s' = ys ++ (tail zs)
                  in rnd_select_aux s' n g'

rnd_select:: [a] -> Int -> IO [a]
rnd_select s n = do
    g <- newStdGen
    return $ fst (rnd_select_aux s n g)
