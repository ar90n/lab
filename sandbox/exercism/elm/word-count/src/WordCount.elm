module WordCount exposing (wordCount)

import Dict exposing (Dict)
import Regex


wordCount : String -> Dict String Int
wordCount sentence =
    let
        de_aps =
            Maybe.withDefault Regex.never <| Regex.fromString "\\s'([^']*)'"
        del = 
            Maybe.withDefault Regex.never <| Regex.fromString "[\\s\\,]"

        rep =
            .match
                >> String.toList
                >> List.filter (\c -> c /= '\'')
                >> String.fromList

        ms =
            Regex.replace de_aps rep sentence
    in
    ms
    |> String.toLower
    |> String.toList
    |> List.filter (\x -> Char.isAlphaNum x || List.member x ['\'', ' ', '\t', '\n', ','])
    |> String.fromList
    |> Regex.split del
    |> List.filter (String.isEmpty >> not)
    |> List.foldl (\w d -> Dict.update w (\v -> (Maybe.withDefault 0 v) + 1 |> Just) d) Dict.empty
