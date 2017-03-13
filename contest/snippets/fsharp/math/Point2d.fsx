// math/Geometry2d.fsx
module Point2d =
    open Geometry2d
    type ^a point = ^a vec
    type relation =
        | COUNTER_CLOCKWISE
        | CLOCKWISE
        | ONLINE_BACK
        | ONLINE_FRONT

    let eps = 1e-12
    let inline direction (p0 : ^a point) (p1 : ^a point) : ^a vec = { x= p1.x - p0.x; y = p1.y - p0.y }
    let inline distanceL1 v0 v1 = direction v0 v1 |> normL1
    let inline distanceL2 v0 v1 = direction v0 v1 |> normL2
    let inline relation (p0 : ^a point) (p1 : ^a point) (p2 : ^a point) : relation =
        let d1 = direction p0 p1
        let d2 = direction p0 p2
        let cross_value = cross d1 d2 |> float
        let dot_value = dot d1 d2 |> float
        match true with
        | _ when eps < cross_value -> COUNTER_CLOCKWISE
        | _ when cross_value < -eps -> CLOCKWISE
        | _ when dot_value < -eps -> ONLINE_BACK
        | _ -> ONLINE_FRONT
