// math/Vector2d.fsx
// math/Point2d.fsx
type line2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                 and ^a : (static member (-) : ^a * ^a -> ^a)
                 and ^a : (static member (*) : ^a * ^a -> ^a)
                 and ^a : (static member (/) : ^a * ^a -> ^a)
                 and ^a : (static member (~-) : ^a -> ^a)
                 and ^a : (static member Abs : ^a -> ^a)
                 and ^a : (static member op_Explicit : ^a -> float)
                 and ^a : comparison> =  ^a point2d * ^a vector2d
module Line2d =
    open Vector2d
    open Point2d
    let inline distanceFromPointL2 (line : ^a line2d) (point  : ^a point2d) : float =
        let (p, v0) = line
        direction p point
        |> cross v0
        |> (fun s -> (float s) / (normL2 v0))
        |> abs

