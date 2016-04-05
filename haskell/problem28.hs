import Data.List

lsort:: [[a]] -> [[a]]
lsort xs = sortOn (\a-> (length a) ) xs

lfsort:: [[a]] -> [[a]]
lfsort xs = let fm = map (\a -> (head a,length a) ) $ group $ map length $ lsort xs
            in sortOn (\a-> lookup (length a) fm) xs
