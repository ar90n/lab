slice:: [a] -> Int -> Int -> [a]
slice x b e =  drop (b-1) $ take e x
