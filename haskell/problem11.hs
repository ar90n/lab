import Data.List

encode:: String -> [(Int,Char)]
encode xs = [(length x,head x) | x <- group xs ]

data RunLength = Multiple Int Char | Single Char
                deriving Show

encodeModified:: String -> [ RunLength ]
encodeModified = map aux . encode
                 where aux (n,c) = case n of
                                        1 -> Single c
                                        _ -> Multiple n c
