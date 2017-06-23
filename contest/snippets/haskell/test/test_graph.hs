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

bellman_ford :: Vertex -> Graph -> Maybe (Array Vertex Cost)
bellman_ford s g = if ak==go ak then Just ak else Nothing where
    k = uncurry subtract $ bounds g
    ak = (!! (k-1)) $ iterate go a0 -- TBD: early stopping 
    c0 = maxBound :: Cost
    a0 = array (bounds g) ((s,0):[(v,c0) | v<-vertices g, v/=s])
    gt = transposeG g
    go :: Array Vertex Cost -> Array Vertex Cost
    go a = a // [(v, f v) | v<-vertices g] where
        f v = minimum $ a!v : [a!w+c | (w,c)<-gt!v, a!w /= maxBound]

bfs :: Graph -> [Vertex] -> [Vertex]
bfs g vs = runST $ do
    visited <- newArray (bounds g) False :: (ST s (STArray s Vertex Bool ))
    foldM (\a v -> (a ++) <$> doit visited (S.singleton v) ) [] vs where
        doit :: STArray s Vertex Bool -> S.Seq Vertex -> ST s ([Vertex])
        doit visited candidates = case S.viewl candidates of
            S.EmptyL -> return []
            c :< cs  -> do
                isVisited <- readArray visited c
                if isVisited then do
                    ncs <- doit visited cs
                    return ncs
                else do
                    writeArray visited c True
                    let candidates' = cs >< (S.fromList $ map fst (g!c))
                    ncs <- doit visited candidates'
                    return (c : ncs)

dff :: Graph -> [Vertex] -> Forest Vertex
dff g vs = prune (bounds g) (map (generate g) vs) where
    generate :: Graph -> Vertex -> Tree Vertex
    generate g v = Node v (map (generate g . fst) (g!v))
    prune :: Bounds -> Forest Vertex -> Forest Vertex
    prune bnds ts = runST $ do
        visited <- (newArray bnds False)::(ST s (STArray s Vertex Bool))
        chop visited ts
    chop :: STArray s Int Bool -> Forest Vertex -> ST s (Forest Vertex)
    chop _ [] = return []
    chop visited (Node v ts : us) = do
        isVisited <- readArray visited v
        if isVisited then
            chop visited us
        else do
            writeArray visited v True
            as <- chop visited ts
            bs <- chop visited us
            return (Node v as : bs)

dfs :: Graph -> [Vertex] -> [Vertex]
dfs g vs = runST $ do
    visited <- newArray (bounds g) False :: (ST s (STArray s Vertex Bool ))
    foldM (\a v -> (a ++) <$> doit visited (S.singleton v) ) [] vs where
        doit :: STArray s Vertex Bool -> S.Seq Vertex -> ST s ([Vertex])
        doit visited candidates = case S.viewl candidates of
            S.EmptyL -> return []
            c :< cs  -> do
                isVisited <- readArray visited c
                if isVisited then do
                    ncs <- doit visited cs
                    return ncs
                else do
                    writeArray visited c True
                    let candidates' = (S.fromList $ map fst (g!c)) >< cs
                    ncs <- doit visited candidates'
                    return (c : ncs)

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

reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dff g [v]) where
    preorder' :: Tree a -> [a] -> [a]
    preorder' (Node a ts) = (a :) . preorderF' ts
    preorderF' :: Forest a -> [a] -> [a]
    preorderF' ts = foldr (.) id $ map preorder' ts
    preorderF :: Forest a -> [a]
    preorderF ts = preorderF' ts []

path :: Graph -> Vertex -> Vertex -> Bool
path g v w = elem w (reachable g v)

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

topSort :: Graph -> [Vertex]
topSort = reverse . postOrd where
    postOrd :: Graph -> [Vertex]
    postOrd g = postorderF (dff g (vertices g)) []
    postorder :: Tree a -> [a] -> [a]
    postorder (Node a ts) = postorderF ts . (a :)
    postorderF   :: Forest a -> [a] -> [a]
    postorderF ts = foldr (.) id $ map postorder ts

warshall_floyd :: Graph -> Maybe(Array (Vertex, Vertex) Cost)
warshall_floyd g = checkResult $ runSTArray $ do
    costs <- newArray ((l,l),(h,h)) maxBound :: ST s (STArray s (Vertex, Vertex) Cost)
    mapM_ (initCost costs) $ [((v,v), 0) | v<-vertices g] ++ [((u,v), c) | (u,v,c)<-edges g]
    return costs
    doit costs (vertices g)
    where
        (l,h) = bounds g
        checkResult :: Array (Vertex,Vertex) Cost -> Maybe(Array (Vertex, Vertex) Cost)
        checkResult costs = let isValid = all (==0) $ [ costs ! (i,i) | i <- [l..h] ]
                            in if isValid then Just costs else Nothing
        initCost :: STArray s (Vertex, Vertex) Cost -> ((Vertex,Vertex), Cost) -> ST s ()
        initCost costs ((f,t),c) = do
            c' <- readArray costs (f,t)
            writeArray costs (f,t) (min c c')
        updateCost :: STArray s (Vertex, Vertex) Cost -> Vertex -> (Vertex,Vertex)-> ST s ()
        updateCost costs k (f,t) = do
            c0 <- readArray costs (f,t)
            c1 <- readArray costs (f,k)
            c2 <- readArray costs (k,t)
            when ((c1 /= maxBound) && (c2 /= maxBound)) $ writeArray costs (f,t) (min c0 (c1+c2))
        doit :: STArray s (Vertex, Vertex) Cost -> [Vertex] -> ST s (STArray s (Vertex, Vertex) Cost)
        doit costs [] = return costs
        doit costs (v:vs) = do
            mapM_ (updateCost costs v) $ [(i,j) | i <- [l..h], j <- [l..h]]
            doit costs vs

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let g = buildG (0,5) [(0,1,40), (0,2,15), (1,0,40), (1,2,20), (1,3,10), (1,4,25), (1,5,6), (2,0,15), (2,1,20), (2,3,100), (3,1,10), (3,2,100), (4,1,25), (4,5,8), (5,1,6), (5,4,8) ]
    let sssp_exp = listArray (0,5) [0,35,15,45,49,41] :: (Array Int Int)
    let Just bellman_ford_result = (\a -> a == sssp_exp) <$> bellman_ford 0 g
    let Just dijkstra_result = (\a -> a == sssp_exp) <$> dijkstra 0 g
    let wr = warshall_floyd g
    putStr "OK\n"

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
