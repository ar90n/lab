split_combinations:: Int -> [a] -> [a] -> [([a],[a])]
split_combinations 0 s m = [([],s ++ m)]
split_combinations _ [] _ = []
split_combinations n (x:xs) ys = (map (\a ->((x:(fst a)),(snd a))) (split_combinations (n-1) xs ys)) ++ (split_combinations n xs (x:ys))

group [] _ = [[]]
group (n:ns) x =  [ ys:gs | (ys,zs) <- (split_combinations n x []), gs <- (group ns zs) ]
