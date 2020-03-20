module PhoneNumber exposing (getNumber)

import Regex


getNumber : String -> Maybe String
getNumber phoneNumber =
    Regex.fromString "^(?:\\+?1(?:\\s+|[-.])?)?\\(?([2-9]\\d\\d)\\)?(?:\\s+|[-.])?([2-9]\\d\\d)(?:\\s+|[-.])?(\\d\\d\\d\\d)\\s*$"
    |> Maybe.withDefault Regex.never
    |> \x -> Regex.find x phoneNumber
    |> List.head
    |> Maybe.andThen (.submatches >> (List.map <| Maybe.withDefault "") >> (List.foldl (\q v-> v ++ q) "") >> Just)
    |> Maybe.andThen (\u -> if String.length u == 10 then Just u else Nothing)
