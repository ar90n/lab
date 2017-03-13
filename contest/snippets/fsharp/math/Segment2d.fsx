// math/Vector2d.fsx
// math/Point2d.fsx
type segment2d< ^a when ^a : (static member (+) : ^a * ^a -> ^a)
                    and ^a : (static member (-) : ^a * ^a -> ^a)
                    and ^a : (static member (*) : ^a * ^a -> ^a)
                    and ^a : (static member (/) : ^a * ^a -> ^a)
                    and ^a : (static member (~-) : ^a -> ^a)
                    and ^a : (static member Abs : ^a -> ^a)
                    and ^a : (static member op_Explicit : ^a -> float)
                    and ^a : comparison> = ^a point2d * ^a point2d
module Segment2d =
    open Vector2d
    open Point2d

    let inline isIntersect (seg0 : ^a segment2d) (seg1 : ^a segment2d) : bool =
        let (p0,p1) = seg0
        let (p2,p3) = seg1
        let check (p0 : ^a vector2d) (p1 : ^a vector2d) (p2 : ^a vector2d) (p3 : ^a vector2d) : bool =
            match (relation p0 p1 p2, relation p0 p1 p3) with
            | (COUNTER_CLOCKWISE, CLOCKWISE)
            | (CLOCKWISE, COUNTER_CLOCKWISE)
            | (ONLINE_FRONT, _)
            | (_, ONLINE_FRONT) -> true
            | _ -> false
        (check p0 p1 p2 p3) && (check p2 p3 p0 p1)

    let inline distanceFromPointL2 (seg : ^a segment2d) (point2d : ^a point2d) : float =
        let (p0,p1) = seg
        let v0 = direction p0 p1
        let v1 = direction p0 point2d
        let v2 = direction p1 point2d
        match true with
        | _ when dot v0 v1 < Core.LanguagePrimitives.GenericZero -> distanceL2 p0 point2d
        | _ when dot (-v0) v2 < Core.LanguagePrimitives.GenericZero -> distanceL2 p0 point2d
        | _ -> cross v0 v1 |> (fun s -> (float s) / (normL2 v0)) |> abs

    let inline distanceL2 (seg0 : ^a segment2d) (seg1 : ^a segment2d) : float =
        let (p0,p1) = seg0
        let (p2,p3) = seg1
        if isIntersect seg0 seg1 then 0.0
        else min (min (distanceFromPointL2 seg0 p2) (distanceFromPointL2 seg0 p3)) (min (distanceFromPointL2 seg1 p0) (distanceFromPointL2 seg1 p1))

    let inline intersecxtPoint (seg0 : ^a segment2d) (seg1 : ^a segment2d) : (float point2d) option =
        if not (isIntersect seg0 seg1) then None
        else
            let (p0,p1) = seg0
            let (p2,p3) = seg1
            let v0 = direction p0 p1
            let v1 = direction p2 p3 |> toFloat
            let d0 = cross v0 <| direction p0  p2 |> abs |> float
            let d1 = cross v0 <| direction p0  p3 |> abs |> float
            let t = d0 / ( d0 + d1 )
            Some ((toFloat p2) + v1 * t)
