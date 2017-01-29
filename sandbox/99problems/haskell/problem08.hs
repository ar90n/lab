aux a b
  | b == [] = [a]
  | a == head b = b
  | otherwise = a:b

compress:: String -> String
compress x = foldr aux "" x

compress2:: String -> String
compress2 x = let aux2 a b
 			| b == [] = [a]
 			| a == head b = b
 			| otherwise = a:b
              in foldr aux2 "" x

compress3:: String -> String
compress3 x = foldr aux2 "" x
	      where aux2 a b = case b of
				[] -> [a]
				h:t  -> if h == a then b  else a:b

