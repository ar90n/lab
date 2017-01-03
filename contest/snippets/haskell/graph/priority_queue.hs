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
