module List =
    let evenodd a = List.foldBack (fun (l,r) x -> x::r,l) a ([],[])
