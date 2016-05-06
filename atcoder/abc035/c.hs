{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Array.IArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

doit:: Int -> [(Int,Int)] -> [Char]
doit n s = elems $ runSTUArray $ do
    lst <- newArray(0,n+1) (0::Int) :: ST s (STUArray s Int Int)
    forM_ s (\(b,e) -> do
                let e' = e + 1
                tb <- readArray lst b 
                te <- readArray lst e'
                writeArray lst b (tb+1)
                writeArray lst e' (te+1))

    forM_ [1..n+1] (\i -> do
                t0 <- readArray lst (i-1)
                t1 <- readArray lst i
                writeArray lst i (t0+t1))

    res <- newArray(0,n-1) '0'
    forM_ [1..n] (\i -> do
                t <- readArray lst i
                when (odd t) $ do
                    writeArray res (i-1) '1')
    return res

main = do
    [n,q] <- (map ( fst . fromJust . BS.readInt) . BS.words) <$> BS.getLine
    s <- replicateM q (((\(a:b:[]) -> ( (fst . fromJust . BS.readInt) a, (fst . fromJust . BS.readInt) b)) . BS.words) <$> BS.getLine)
    let res = doit n s
    putStrLn res
