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
