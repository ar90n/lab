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

data Heap a = Empty | Heap Int a (Heap a) (Heap a) deriving Show

merge' :: (a -> a -> Bool) -> Heap a -> Heap a -> Heap a
merge' _ h     Empty = h
merge' _ Empty h     = h
merge' f h1@(Heap _ x left1 right1) h2@(Heap _ y left2 right2)
  | f x y     = makeHeap x left1 (merge' f right1 h2)
  | otherwise = makeHeap y left2 (merge' f right2 h1)
  where
    makeHeap :: a -> Heap a -> Heap a -> Heap a
    makeHeap x Empty a@(Heap _ _ _ _) = Heap 1 x a Empty
    makeHeap x a@(Heap _ _ _ _) Empty = Heap 1 x a Empty
    makeHeap x a@(Heap ra _ _ _) b@(Heap rb _ _ _) = if ra <= rb then Heap (ra+1) x b a else Heap (rb+1) x a b

singleton' :: a -> Heap a
singleton' x = Heap 1 x Empty Empty

push' :: (a -> a-> Bool) -> Heap a -> a -> Heap a
push' f h x = merge' f (singleton' x) h

pop' :: (a -> a-> Bool) -> Heap a -> (a, Heap a)
pop' _ Empty = error "Empty Heap"
pop' f (Heap _ x a b) = (x,  ( merge' f a b))

data PriorityQueue a = PriorityQueue (a -> a -> Bool) (Heap a)

instance Show a => Show (PriorityQueue a) where
    show (PriorityQueue _ h) = show h

singleton :: (a -> a -> Bool ) -> a -> PriorityQueue a
singleton f x = PriorityQueue f (singleton' x)

push :: PriorityQueue a -> a -> PriorityQueue a
push (PriorityQueue f h) x = PriorityQueue f (push' f h x)

pop :: PriorityQueue a -> (a, PriorityQueue a)
pop (PriorityQueue f h) = let (x,h') = pop' f h
                          in (x, PriorityQueue f h')

top :: PriorityQueue a -> a
top (PriorityQueue _ Empty) = error "Empty Queue"
top (PriorityQueue _ (Heap _ x _ _)) = x

isEmpty :: PriorityQueue a -> Bool
isEmpty (PriorityQueue _ Empty) = True
isEmpty _ = False

fromList :: (a -> a -> Bool) -> [a] -> PriorityQueue a
fromList f xs = PriorityQueue f (foldl (push' f) Empty xs)

toList :: PriorityQueue a -> [a]
toList (PriorityQueue _ Empty) = []
toList (PriorityQueue f h) = toList' f h where
    toList' :: (a -> a -> Bool) -> Heap a -> [a]
    toList' f h = let (x, h') = pop' f h
                  in x : toList' f h'

dijkstra :: Vertex -> Graph -> Maybe (Array Vertex Cost)
dijkstra s g = Just $ runSTArray $ do
    costs <- newArray (bounds g) maxBound :: (ST s (STArray s Vertex Cost))
    let pqueue = singleton (\(v0,c0) (v1,c1) -> c0 < c1) (s,0)
    doit costs pqueue where
    doit :: STArray s Vertex Cost -> PriorityQueue (Vertex,Cost) -> ST s (STArray s Vertex Cost)
    doit costs queue | isEmpty queue = return costs
                     | otherwise     = do
                            let ((v,c),queue') = pop queue
                            c' <- readArray costs v
                            if (c < c') then do
                                writeArray costs v c
                                let queue'' = foldl (\q (v,cc) -> push q (v,c+cc)) queue (g!v)
                                doit costs queue''
                            else do
                                doit costs queue'

main :: IO ()
main = do
    (n,m,t) <- readInt3 <$> BS.getLine
    s <- readIntN <$> BS.getLine
    d <- replicateM m ((\s -> readInt3 s) <$> BS.getLine )
    let g = buildG (1,n) d
        g' = transposeG g
        (Just c) = Data.Array.IArray.assocs <$> dijkstra 1 g
        (Just c') = Data.Array.IArray.assocs <$> dijkstra 1 g'
        s2 = maximum [ ss * ( t -  (c0 + c1) )  | ((_,c0),(_,c1), ss) <- (zip3 c c' s) , c0 /= (maxBound :: Cost) , c1 /= (maxBound :: Cost) ]
    putStrLn ( show s2 )

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

