module AtbashCipher exposing (decode, encode)


encdec : List Char -> List Char
encdec =
    List.filter Char.isAlphaNum
        >> List.map Char.toLower
        >> List.map Char.toCode
        >> List.map (\x -> if (Char.toCode 'a' <= x) && (x <= Char.toCode 'z') then 219 - x else x)
        >> List.map Char.fromCode


encode : String -> String
encode plain =
    plain
        |> String.toList
        |> encdec
        |> List.indexedMap (\i c -> (if 0 < i &&  0 == modBy 5 i then " " else "") ++ (String.fromChar c))
        |> String.concat


decode : String -> String
decode cipher =
    cipher
        |> String.toList
        |> encdec
        |> String.fromList
