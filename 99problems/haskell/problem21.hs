insertAt:: Char -> String -> Int -> String
insertAt c s n = let (a,b) = splitAt (n-1) s
                 in a ++ (c : b)
