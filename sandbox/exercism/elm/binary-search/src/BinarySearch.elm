module BinarySearch exposing (find)

import Array exposing (Array)


biseq : Array Int -> Int -> Int -> Int -> Maybe Int
biseq xs target ls rs =
    let
        mid =
            (ls + rs) // 2
    in
    Array.get mid xs
        |> Maybe.andThen
            (\midv ->
                if midv == target then
                    Just mid

                else if rs <= ls then
                    Nothing

                else if midv < target then
                    biseq xs target (mid + 1) rs
                else
                    biseq xs target ls mid
            )


find : Int -> Array Int -> Maybe Int
find target xs =
    biseq xs target 0 (Array.length xs)
