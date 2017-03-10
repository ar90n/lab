module Line2d =
    open Geometry2d
    open Point2d
    type ^a line = ^a point * ^a vec

    let inline distanceFromPointL2 (line : ^a line) (point  : ^a point) : float =
        let (p, v0) = line
        cross v0 (point - p) |> (fun s -> s / (distanceL2 v0)) |> abs
