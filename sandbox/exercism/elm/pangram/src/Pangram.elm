module Pangram exposing (isPangram)

import Set


isPangram : String -> Bool
isPangram sentence =
    String.toList sentence
        |> List.filterMap
            (\x ->
                if Char.isAlpha x then
                    Just <| Char.toLower x

                else
                    Nothing
            )
        |> Set.fromList
        |> Set.size
        |> (==) 26
