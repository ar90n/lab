dupli:: [a] -> [a]
dupli = foldr (\a b -> a:a:b ) []

