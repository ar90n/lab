import Data.List

repli:: String -> Int -> String
repli a b = foldr (\x y -> replicate b x ++ y ) "" a

