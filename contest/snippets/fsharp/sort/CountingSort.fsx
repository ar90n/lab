let CountingSort (minv : int) (maxv : int) (vs : int list) =
    let bins = maxv - minv + 1
    let cs = Array.create bins 0
    vs
    |> List.iter (fun v -> let i = v - minv
                           cs.[i] <- cs.[i] + 1)
    [1..bins-1]
    |> List.iter (fun i -> cs.[i] <- cs.[i] + cs.[i - 1])

    let res = Array.create (List.length vs) 0
    vs
    |> List.iter (fun v -> res.[cs.[v]-1] <- v
                           cs.[v] <- cs.[v] - 1)
    Array.toList res
