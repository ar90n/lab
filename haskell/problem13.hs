import Data.List

data RunLength = Multiple Int Char | Single Char
                deriving Show

encodeDirect:: String -> [ RunLength ]
encodeDirect = map ( aux2 . aux1 ) .  group
               where aux2 (1,c) = Single c
                     aux2 (n,c) = Multiple n c
                     aux1 x = ( length x, head x )
