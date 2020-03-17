module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    [ ( 3, "Pling" ), ( 5, "Plang" ), ( 7, "Plong" ) ]
        |> List.filterMap
            (\( x, s ) ->
                if modBy x number == 0 then
                    Just s

                else
                    Nothing
            )
        |> List.foldl (\x y -> y ++ x) ""
        |> (\x ->
                if String.isEmpty x then
                    String.fromInt number

                else
                    x
           )
