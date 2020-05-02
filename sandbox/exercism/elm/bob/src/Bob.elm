module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        yell =
            remark |> String.filter Char.isAlpha |> (\x -> (not (String.isEmpty x)) &&  (String.all Char.isUpper x))

        question =
            remark |> String.trim |> String.right 1 |> (==) "?"
    in
    if (String.trim remark) == "" then
        "Fine. Be that way!"
    else
        case ( yell, question ) of
            ( False, True ) ->
                "Sure."

            ( True, False ) ->
                "Whoa, chill out!"

            ( True, True ) ->
                "Calm down, I know what I'm doing!"

            _ ->
                "Whatever."
