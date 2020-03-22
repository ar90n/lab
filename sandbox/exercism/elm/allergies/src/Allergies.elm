module Allergies exposing (Allergy(..), isAllergicTo, toList)

import Bitwise

type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    (case allergy of
        Eggs ->
            1

        Peanuts ->
            2

        Shellfish ->
            4

        Strawberries ->
            8

        Tomatoes ->
            16

        Chocolate ->
            32

        Pollen ->
            64

        Cats ->
            128
    )
        |> Bitwise.and score
        |> (/=) 0


toList : Int -> List Allergy
toList score =
    [ Eggs , Peanuts , Shellfish , Strawberries , Tomatoes , Chocolate , Pollen , Cats]
    |> List.filter (\x -> isAllergicTo x score)
