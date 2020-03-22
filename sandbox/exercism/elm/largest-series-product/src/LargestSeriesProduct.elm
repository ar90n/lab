module LargestSeriesProduct exposing (largestProduct)



largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    let
        ns =
            series |> String.split "" |> List.map String.toInt

        bs =
            List.repeat length (Just 1) ++ ns

        aux ( bb, nn ) ( aacc, l, z ) =
            case ( bb, nn, aacc ) of
                ( Nothing, _, _ ) ->
                    ( Nothing, 0, 0 )

                ( _, Nothing, _ ) ->
                    ( Nothing, 0, 0 )

                ( _, _, Nothing ) ->
                    ( Nothing, 0, 0 )

                ( Just b, Just n, Just acc ) ->
                    if n == 0 then
                        ( Just 1, 0, 0 )

                    else if length <= l then
                        ( Just (n * acc // b), length, z + 1 )

                    else
                        ( Just (n * acc), l + 1, z + 1 )

        sel cv ( acc, l, z ) =
            let
                nv =
                    Maybe.map
                        (\aaa ->
                            if length <= z then
                                aaa

                            else
                                0
                        )
                        acc

                mv =
                    Maybe.map2
                        (\cc nn ->
                            if cc < nn then
                                nn

                            else
                                cc
                        )
                        cv
                        nv
            in
            ( mv, ( acc, l, z ) )
    in
    if length < 0 || String.length series < length then
        Nothing

    else if series == "" then
        if length == 0 then
            Just 1

        else
            Nothing

    else
        List.map2 Tuple.pair bs ns
            |> List.foldl (\c ( mv, s ) -> aux c s |> sel mv) ( Just 0, ( Just 1, 0, 0 ) )
            |> Tuple.first
