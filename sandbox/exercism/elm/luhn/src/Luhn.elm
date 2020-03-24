module Luhn exposing (valid)


valid : String -> Bool
valid input =
    input
        |> String.toList
        |> (\b ->
                let
                    rs =
                        b |> List.filter ((\c -> Char.isDigit c || c == ' ') >> not) |> List.length
                in
                if 0 < rs then
                    Nothing

                else
                    Just b
           )
        |> Maybe.map (List.filter Char.isDigit)
        |> Maybe.andThen
            (\x ->
                if 1 < List.length x then
                    Just x

                else
                    Nothing
            )
        |> Maybe.map (List.map (\y -> Char.toCode y - Char.toCode '0'))
        |> Maybe.map (\nn -> ( nn, List.length nn |> modBy 2 ))
        |> Maybe.map
            (\( vvv, ccc ) ->
                vvv
                    |> List.indexedMap
                        (\i v ->
                            if ccc == modBy 2 i then
                                2 * v

                            else
                                v
                        )
            )
        |> Maybe.map
            (List.map
                (\z ->
                    if 10 <= z then
                        z - 9

                    else
                        z
                )
            )
        |> Maybe.map List.sum
        |> Maybe.andThen
            (\a ->
                if 0 == modBy 10 a then
                    Just True

                else
                    Nothing
            )
        |> Maybe.withDefault False
