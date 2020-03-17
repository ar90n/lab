module Hamming exposing (distance)


distance : String -> String -> Result String Int
distance left right =
    let
        left_chars =
            String.toList left

        right_chars =
            String.toList right
    in
    if List.length left_chars /= List.length right_chars then
        Err "left and right strands must be of equal length"

    else
        List.map2 (==) left_chars right_chars
            |> List.map
                (\x ->
                    if x then
                        0

                    else
                        1
                )
            |> List.foldl (+) 0
            |> Ok
