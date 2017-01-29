split:: String -> Int -> [String]
split x n  = aux ["",x] n
             where aux (a:b) 0 = (reverse a):b
                   aux (a:b) n = let c = head b
                                 in aux (((head c):a):[tail c]) (n-1)
