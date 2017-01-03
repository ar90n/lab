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
