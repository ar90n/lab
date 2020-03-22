module Triangle exposing (rows)


rows : Int -> List (List Int)
rows n =
    let
        aux acc c =
            if c <= 0 then
                Nothing

            else if c == 1 then
                Just acc

            else
                List.head acc
                    |> Maybe.map (\x -> ( List.drop 1 x, x ))
                    |> Maybe.map (\( b, m ) -> List.map2 (+) b m)
                    |> Maybe.map (\y -> (1 :: y) |> List.reverse |> (\z -> 1 :: z))
                    |> Maybe.andThen (\a -> aux (a :: acc) (c - 1))
    in
    aux [ [ 1 ] ] n |> Maybe.withDefault [] |> List.reverse
