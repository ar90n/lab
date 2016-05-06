import Data.List
import Control.Monad

main = do
    line <- getLine
    let xs = sort . map (\a -> read a::Int) . words $  line
        lt = head xs
        gt = last xs
        is43 = ( 4 * lt ) == ( 3 * gt )
    when is43 $ putStrLn "4:3"
    when (not is43) $ putStrLn "16:9"
