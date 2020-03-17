module SumOfMultiples exposing (sumOfMultiples)

import Set


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    let
        candidates n i =
            if limit <= (n * i) then
                [ 1 ]

            else
                (n * i) :: candidates n (i + 1)

        aux divs acc =
            case divs of
                hd :: tl ->
                    candidates hd 1
                        |> List.filterMap
                            (\x ->
                                if acc * x < limit then
                                    Just (acc * x)

                                else
                                    Nothing
                            )
                        |> List.map (aux tl)
                        |> List.foldl (\a b -> a ++ b) [ acc ]

                [] ->
                    [ acc ]
    in
    aux divisors 1 |> Set.fromList |> Set.foldl (+) 0 |> (\x -> x - 1)



-- aux divisors 0 |> List.foldl (+) 0
