module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    let
        cs =
            String.toList input

        aux acc lst =
            List.take size lst
                |> (\x ->
                        if List.length x == size then
                            Just x

                        else
                            Nothing
                   )
                |> Maybe.map (\y -> aux (y :: acc) (List.drop 1 lst))
                |> Maybe.withDefault acc
    in
    Ok input
        |> Result.andThen (\a -> if (not (String.isEmpty a)) then Ok a else Err "series cannot be empty")
        |> Result.andThen (\x -> if size <= (String.length x) then Ok x else Err "slice length cannot be greater than series length")
        |> Result.andThen (\y -> if size /= 0 then Ok y else Err "slice length cannot be zero")
        |> Result.andThen (\z -> if 0 < size  then Ok z else Err "slice length cannot be negative")
        |> Result.map (String.toList >> List.map (Char.toCode >> (\x -> x - Char.toCode '0')))
        |> Result.map (aux [])
        |> Result.map List.reverse
