module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)


squareOfSum : Int -> Int
squareOfSum n =
    List.range 0 n |> List.sum |> (\x -> x * x)


sumOfSquares : Int -> Int
sumOfSquares n =
    List.range 0 n |> List.map (\x -> x * x) |> List.sum


difference : Int -> Int
difference n =
    (squareOfSum n) - (sumOfSquares n)
