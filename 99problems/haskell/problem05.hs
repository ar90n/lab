myReverse ::[a] -> [a]
myReverse x = foldl (flip (:)) [] x
