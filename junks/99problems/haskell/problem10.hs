import Data.List

encode:: String -> [(Int,Char)]
encode = map (\x ->(length x, head x)) . group
