// math/Vector2d.fsx
type relation2d =
    | COUNTER_CLOCKWISE
    | CLOCKWISE
    | ONLINE_BACK
    | ONLINE_FRONT
type point2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                  and ^a : (static member (-) : ^a * ^a -> ^a)
                  and ^a : (static member (*) : ^a * ^a -> ^a)
                  and ^a : (static member (/) : ^a * ^a -> ^a)
                  and ^a : (static member (~-) : ^a -> ^a)
                  and ^a : (static member Abs : ^a -> ^a)
                  and ^a : (static member op_Explicit : ^a -> float)
                  and ^a : comparison> = ^a vector2d
module Point2d =
    open Vector2d
    let eps = 1e-12
    let inline direction (p0 : ^a point2d) (p1 : ^a point2d) : ^a vector2d = { x= p1.x - p0.x; y = p1.y - p0.y }
    let inline distanceL1 v0 v1 = direction v0 v1 |> normL1
    let inline distanceL2 v0 v1 = direction v0 v1 |> normL2
    let inline relation (p0 : ^a point2d) (p1 : ^a point2d) (p2 : ^a point2d) : relation2d =
        let d1 = direction p0 p1
        let d2 = direction p0 p2
        let cross_value = cross d1 d2 |> float
        let dot_value = dot d1 d2 |> float
        match true with
        | _ when eps < cross_value -> COUNTER_CLOCKWISE
        | _ when cross_value < -eps -> CLOCKWISE
        | _ when dot_value < -eps -> ONLINE_BACK
        | _ -> ONLINE_FRONT
