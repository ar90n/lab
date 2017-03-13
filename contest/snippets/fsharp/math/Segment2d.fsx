// math/Geometry2d.fsx
// math/Point2d.fsx
module Segment2d =
    open Geometry2d
    open Point2d
    type ^a segment = ^a point * ^a point

    let inline isIntersect (seg0 : ^a segment) (seg1 : ^a segment) : bool =
        let (p0,p1) = seg0
        let (p2,p3) = seg1
        let check (p0 : ^a vec) (p1 : ^a vec) (p2 : ^a vec) (p3 : ^a vec) : bool =
            match (relation p0 p1 p2, relation p0 p1 p3) with
            | (COUNTER_CLOCKWISE, CLOCKWISE)
            | (CLOCKWISE, COUNTER_CLOCKWISE)
            | (ONLINE_FRONT, _)
            | (_, ONLINE_FRONT) -> true
            | _ -> false
        (check p0 p1 p2 p3) && (check p2 p3 p0 p1)

    let inline distanceFromPointL2 (seg : ^a segment) (point : ^a point) : float =
        let (p0,p1) = seg
        let v0 = p1 - p0
        let v1 = point - p0
        let v2 = point - p1
        match true with
        | _ when dot v0 v1 < Core.LanguagePrimitives.GenericZero -> distanceL2 p0 point
        | _ when dot (-v0) v2 < Core.LanguagePrimitives.GenericZero -> distanceL2 p0 point
        | _ -> cross v0 v1 |> (fun s -> s / (distanceL2 v0)) |> abs

    let inline distanceL2 (seg0 : ^a segment) (seg1 : ^a segment) : float =
        let (p0,p1) = seg0
        let (p2,p3) = seg1
        if isIntersect seg0 seg1 then 0.0
        else min (min (distanceFromPointL2 seg0 p2) (distanceFromPointL2 seg0 p3)) (min (distanceFromPointL2 seg1 p0) (distanceFromPointL2 seg1 p1))

    let inline intersecxtPoint (seg0 : ^a segment) (seg1 : ^a segment) : (^a point) option =
        if not (isIntersect seg0 seg1) then None
        else
            let (p0,p1) = seg0
            let (p2,p3) = seg1
            let v0 = p1 - p0
            let d0 = cross v0 (p2 - p0) |> abs
            let d1 = cross v0 (p3 - p0) |> abs
            let t = d0 / ( d0 + d1 )
            Some (p2 + (p3 - p2) * t)

