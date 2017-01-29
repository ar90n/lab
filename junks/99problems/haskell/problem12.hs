import Data.List

data RunLength = Multiple Int Char | Single Char deriving Show

decodeModified:: [ RunLength ] -> String
decodeModified = foldr aux ""
                 where aux a b = case a of
                                    Multiple n c -> ( take n ( repeat c ) ) ++ b
                                    Single c -> c : b
