module Acronym exposing (abbreviate)

import Regex

abbreviate : String -> String
abbreviate phrase =
    let
        delimiter = Maybe.withDefault Regex.never <| Regex.fromString "[ ,-]+"
    in
        Regex.split delimiter phrase
        |> List.filterMap (String.toList >> List.head)
        |> List.map Char.toUpper
        |> String.fromList
