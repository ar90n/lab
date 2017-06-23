module List =
    let evenodd a = List.foldBack (fun (l,r) x -> x::r,l) a ([],[])
    let splitBy2 f xs =
        List.fold (fun acc x -> match acc with
                                | [] -> [[x]]
                                | (h::t) when  f (List.head h) x -> [x]::h::t
                                | (h::t) -> (x::h)::t) [] xs
        |> List.rev
