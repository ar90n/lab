import System.Random
import Data.List

rnd_permu:: [a] -> IO [a]
rnd_permu s = do
    let permus = permutations [0..((length s)-1)]
    g <- newStdGen
    let (i,_) = randomR (0,(length permus)-1) g
    return $ [ s !! x | x <- (permus !! i) ]
