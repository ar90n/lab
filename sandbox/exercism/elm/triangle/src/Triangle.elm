module Triangle exposing (Triangle(..), triangleKind)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    let
        maxv = max x y |> max z
        minv = min x y |> min z
        midv = x + y + z - maxv - minv
    in
        if minv <= 0 then
            Err "Invalid lengths"
        else if (minv + midv) < maxv then
            Err "Violates inequality"
        else if maxv == minv then
            Ok Equilateral
        else if maxv == midv then
            Ok Isosceles
        else
            Ok Scalene
