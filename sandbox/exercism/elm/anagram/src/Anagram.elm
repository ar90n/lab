module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    let
        sort_str = String.toList >> List.map Char.toLower >> List.sort >> String.fromList
        lower_word = String.toLower word
        sorted_word = sort_str word
    in
        candidates |> List.filter (\s -> sorted_word == sort_str s && lower_word /= String.toLower s)
