module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    let
        aux c n =
            if n <= 0 then
                Err "Only positive numbers are allowed"

            else if n == 1 then
                Ok c

            else
                (if modBy 2 n == 0 then
                    n // 2

                 else
                    3 * n + 1
                )
                    |> aux (c + 1)
    in
    aux 0 start
