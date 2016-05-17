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
