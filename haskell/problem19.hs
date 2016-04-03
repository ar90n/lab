rotate:: [a] -> Int -> [a]
rotate x n = let len = length x
                 m = ( n + len ) `mod` len
             in (drop m x) ++ ( take m x )
