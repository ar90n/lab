module Array =
    let evenodd a = Array.foldBack (fun (l,r) x -> x::r,l) a ([],[])
