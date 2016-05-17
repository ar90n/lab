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
import           Data.Array
--import           Data.Array.Unboxed
--import           Data.Array.IArray
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
type Edge = (Vertex, Vertex, Cost)
type Cost = Int
type Vertex = Int

type Graph = Table [(Vertex,Cost)]
type Table a = Array Vertex a
type Bounds = (Vertex, Vertex)

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [(u,v,c) | u <- vertices g, (v,c) <- g!u]

costs :: Graph -> [Cost]
costs g = [c | u <- vertices g, (_,c) <- g!u]
dists = costs
weighs = costs

cost :: Edge -> Cost
cost (_,_,c) = c
dist = cost
weight = cost

mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]

buildG :: Bounds -> [Edge] -> Graph
buildG bs = accumArray (flip (:)) [] bs . map out where
    out (u,v,c) = (u,(v,c))

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [(v,u,c) | (u,v,c) <- edges g]

outdegree :: Graph -> Table Int
outdegree = mapT (const length)

indegree :: Graph -> Table Int
indegree = outdegree . transposeG

den :: Int
den = 1000000000 + 7

doit :: Graph -> Vertex -> Int
doit g v =  (w + b) `mod` den where
    (b,w) = doit' 0 v
    doit' :: Vertex -> Vertex -> (Int, Int)
    doit' p v = ( b', w' ) where
        children = [ doit' v v' | (v',_) <- g!v, v' /= p]
        b' = if null children then 1 else foldl1 (\a b -> (a * b ) `mod` den) . map snd $ children
        w' = if null children then 1 else foldl1 (\a b -> (a * b ) `mod` den) . map (\(a,b) -> (a + b) `mod` den ) $ children

main :: IO ()
main = do
    n <- readInt1 <$> BS.getLine
    edges <- replicateM (n-1) $ (\(a,b) -> (a,b,0)) . readInt2 <$>  BS.getLine
    let edges' = [ (b,a,c) | (a,b,c) <- edges ]
        g = buildG (1,n) (edges ++ edges')
        res = doit g 1
    putStrLn (show res )


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

