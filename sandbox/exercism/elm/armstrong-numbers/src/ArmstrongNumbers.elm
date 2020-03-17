module ArmstrongNumbers exposing (isArmstrongNumber)


isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    let
        ns = String.fromInt nb |> String.toList
        nc = List.length ns
    in
        ns
        |> List.map (\x -> (Char.toCode x) - (Char.toCode '0'))
        |> List.map (\x -> x ^ nc)
        |> List.foldl (+) 0
        |> (==) nb

