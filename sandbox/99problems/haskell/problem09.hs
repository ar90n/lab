import Data.List

pack:: [Char] -> [String]
pack = group . foldr (:) []

pack2:: [Char] -> [String]
pack2 x = let aux a b
		| b == [] = [[a]]
		| a == head ( head b ) = (a:( head b)):(tail b )
		| otherwise =[a] : b
	in foldr aux [] x

pack3:: [Char] -> [String]
pack3 [] = []
pack3 [x] = [[x]]
pack3 (x:xs) = if x `elem` (head (pack3 xs))
	       then (x:(head (pack3 xs))):(tail (pack xs))
	       else [x]:(pack xs)
