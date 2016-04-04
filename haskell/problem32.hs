myGCD:: Int -> Int -> Int
myGCD x 0 = x
myGCD x y = myGCD y (x `mod` y )
