dropEvery:: String -> Int -> String
dropEvery x n = aux x 1
                where aux "" _ = ""
                      aux (x:xs) m = if( m `mod` n == 0 ) then aux xs (m+1)
                                                          else x : ( aux xs (m+1))
