// math/Geometry2d.fsx
// math/Point2d.fsx
type ContainStatus =
    | OUT
    | ON
    | IN

let PolygonContains (polygon : (^a point) list) (point : ^a point) : ContainStatus =
    let eps = 1e-12
    let h = List.head polygon
    let doit (p0 : ^a point) (p1 : ^a point) =
        let a = if p0.y < p1.y then p0 else p1
        let b = if p0.y < p1.y then p1 else p0
        if ((((float >> abs) (cross a b)) < eps) && ((float (dot a b)) < eps)) then (1,0)
        else if ( ((float a.y) < eps) && (eps < (float b.y)) && (eps < (float (corss a b)))) then (0,1)
        else (0,0)

    List.rev polygon
    |> List.fold (fun (l,p0) p1 -> ((p0,p1)::l,p1)) ([],h)
    |> fst
    |> List.fold (fun (oc,cc) (p0,p1) -> let (no,nc) = doit p0 p1
                                         (oc + no, cc + nc)) (0,0)
    |> (fun (oc,cc) -> match (oc,cc) with
                       | _ when 0 < oc -> ON
                       | _ when 0 < cc -> IN
                       | _ -> OUT)
