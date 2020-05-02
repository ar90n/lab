module Isogram exposing (isIsogram)


isIsogram : String -> Bool
isIsogram sentence =
    String.toList sentence
    |> List.filter Char.isAlpha
    |> List.map Char.toLower
    |> List.sort
    |> \x -> (List.drop 1 x, List.take ((List.length x) - 1) x)
    |> \(y, z) -> List.map2 (\a b -> a /= b) y z
    |> List.foldl (&&) True