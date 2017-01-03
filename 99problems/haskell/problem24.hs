import System.Random

diff_select:: Int -> Int -> IO [Int]
diff_select n m = do
    g <- newStdGen
    return $ take n $ randomRs (1,m) g
