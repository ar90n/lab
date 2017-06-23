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
import           Control.Monad.ST
import           Data.Array.ST
-- import           Data.Array.Unsafe
-- import           Data.Vector.Unboxed ((//), (++), (!), (!?))
-- import qualified Data.Vector.Unboxed as U
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
import           Data.Sequence ((|>), (<|), (><),ViewR((:>)), ViewL((:<)))
import qualified Data.Sequence as S
import           Debug.Trace

--------------------------------------------------------------------------------

binarySearch :: Ord a => Array Int a -> a -> Maybe Int
binarySearch haystack needle = binarySearch' haystack needle lo hi where
    (lo,hi) = bounds haystack
    binarySearch' :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
    binarySearch' haystack needle lo hi
        | hi < lo        = Nothing
        | pivot > needle = binarySearch' haystack needle lo (mid-1)
        | pivot < needle = binarySearch' haystack needle (mid+1) hi
        | otherwise      = Just mid
        where
            mid   = lo + (hi-lo) `div` 2
            pivot = haystack!mid

lowerBound :: Ord a => Array Int a -> a -> Int
lowerBound haystack needle = lowerBound' haystack needle lo hi where
    (lo,hi) = bounds haystack
    lowerBound' :: Ord a => Array Int a -> a -> Int -> Int -> Int
    lowerBound' haystack needle lo hi
        | lo == (hi-1) = hi
        | needle <= pivot = lowerBound' haystack needle lo mid
        | pivot < needle = lowerBound' haystack needle mid hi
        where
            mid   = lo + (hi-lo) `div` 2
            pivot = haystack!mid

upperBound :: Ord a => Array Int a -> a -> Int
upperBound haystack needle = upperBound' haystack needle lo hi where
    (lo,hi) = bounds haystack
    upperBound' :: Ord a => Array Int a -> a -> Int -> Int -> Int
    upperBound' haystack needle lo hi
        | lo == (hi-1) = hi
        | needle < pivot = upperBound' haystack needle lo mid
        | pivot <= needle = upperBound' haystack needle mid hi
        where
            mid   = lo + (hi-lo) `div` 2
            pivot = haystack!mid

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let d = listArray (0,10) [1,2,4,4,4,5,6,6,7,10] :: Array Int Int
        Just f = binarySearch d 5
        l = lowerBound d 6
        u = upperBound d 6
    putStrLn (show f)
    putStrLn (show l)
    putStrLn (show u)

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
