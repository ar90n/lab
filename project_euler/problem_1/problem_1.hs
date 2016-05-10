{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE TupleSections #-}

import           System.IO hiding (char8)
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple
import           Data.Tree
import           Data.Int
import           Data.Char
import           Data.Function (on)
-- import           Data.Array
import           Data.Array.Unboxed
import           Data.Array.IArray
import           Data.Ix
import           Data.Maybe
import           Data.Monoid hiding ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Builder
-- import           Control.Monad.ST
-- import           Data.Array.ST
-- import           Data.Array.Unsafe
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
-- import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
-- import qualified Data.Sequence as S
import           Debug.Trace

--------------------------------------------------------------------------------

doit :: Int -> Int
doit n = sum3 + sum5 - sum15 where
    n' = n - 1
    n3 = div n' 3
    n5 = div n' 5
    n15 = div n' 15
    sum3 = n3 * ( 2 * 3 + ( n3 - 1 ) * 3 ) `div` 2
    sum5 = n5 * ( 2 * 5 + ( n5 - 1 ) * 5 ) `div` 2
    sum15 = n15 * ( 2 * 15 + ( n15 - 1 ) * 15 ) `div` 2

main :: IO ()
main = do
    t <- readInt1 <$> BS.getLine
    res <- replicateM t $ readInt1 <$> BS.getLine
    mapM_ putStrLn ( map ( show . doit ) res )
--------------------------------------------------------------------------------

readInt1 :: BS.ByteString -> Int
readInt1 = fst . fromJust . BS.readInt 

readInt2 :: BS.ByteString -> (Int,Int)
readInt2 = toTuple . readIntN

readInt3 :: BS.ByteString -> (Int,Int,Int)
readInt3 = toTriple . readIntN

readIntN :: BS.ByteString -> [Int]
readIntN =  map readInt1 . BS.words

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] =(x, y, z)

fromTuple :: (a, a) -> [a]
fromTuple (x, y) = [x, y]

fromTriple :: (a, a, a) -> [a]
fromTriple (x, y, z) = [x, y, z]

applyTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
applyTuple f g (x, y) = (f x, g y)

applyTriple :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
applyTriple f g h (x, y, z) = (f x, g y, h z)

