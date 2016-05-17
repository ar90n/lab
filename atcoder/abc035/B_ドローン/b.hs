
accCom::(Int,Int,Int,Int,Int) -> Char -> (Int,Int,Int,Int,Int)
accCom (al,au,ar,ad,aq) c | c == 'L' = (al+1,au,ar,ad,aq)
                          | c == 'U' = (al,au+1,ar,ad,aq)
                          | c == 'R' = (al,au,ar+1,ad,aq)
                          | c == 'D' = (al,au,ar,ad+1,aq)
                          | c == '?' = (al,au,ar,ad,aq+1)

main = do
    cLine <- getLine
    tLine <- getLine
    let (al,au,ar,ad,aq) = foldl accCom (0,0,0,0,0) cLine
        dist = abs( al - ar ) + abs( au -ad )
        t = read tLine::Int
        dist' = if t == 1 then dist + aq
                          else if aq < dist then dist - aq 
                                           else (dist - aq) `mod` 2
    putStrLn (show dist')



