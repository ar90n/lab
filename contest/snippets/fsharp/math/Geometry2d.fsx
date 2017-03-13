module Geometry2d =
    type ^a vec =
        {
            x : ^a
            y : ^a
        }

        static member inline (+) (v0, v1) = { x = v0.x + v1.x; y = v0.y + v1.y }
        static member inline (-) (v0, v1) = { x = v0.x - v1.x; y = v0.y - v1.y }
        static member inline (*) (v, k) = { x = k * v.x; y = k * v.y }
        static member inline (*) (k, v) = { x = k * v.x; y = k * v.y }
        static member inline (/) (v, k) = { x = v.x / k; y = v.y / k }
        static member inline (~-) (v) = { x = -v.x; y = -v.y }

    let inline dot (v0 : ^a vec) (v1 : ^a vec) : ^a = v0.x * v1.x + v0.y * v1.y
    let inline cross (v0 : ^a vec) (v1 : ^a vec) : ^a = v0.x * v1.y - v0.y * v1.x
    let inline normL1 (v : ^a vec) : ^a = abs v.x + abs v.y
    let inline normL2 (v : ^a vec) : float = dot v v |> float |> sqrt
    let inline compare (v0 : ^a vec) (v1 : ^a vec) : int = if v0.x <> v1.x then compare v0.x v1.x else compare v0.y v1.y
    let inline isOrthogonal (v0 : ^a vec) (v1 : ^a vec) : bool = dot v0 v1 |> ((=) Core.LanguagePrimitives.GenericZero)
    let inline isParallel (v0 : ^a vec) (v1 : ^a vec) : bool = cross v0 v1 |> ((=) Core.LanguagePrimitives.GenericZero)
    let inline project (v0 : ^a vec) (v1 : ^a vec) : float vec = (float (dot v0 v1)) / (normL2 v0) * ({x = float v0.x; y = float v0.y})
    let inline reflect (v0 : ^a vec) (v1 : ^a vec) : float vec = (project v0 v1) + (project v0 v1) - ({x = float v1.x; y = float v1.y})
