removeAt:: Int -> String -> (Char,String)
removeAt n x = let (a,b) =  splitAt n x
	       in (last a, (init a) ++ b )
