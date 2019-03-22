let anagrams: (string, list(string)) => list(string) = (s, lst) => {
    if(s == Js.String.toUpperCase(s)) {
        []
    } else {
        let sort = s => Js.String.toLowerCase(s)
            |> Js.String.split("")
            |> Js.Array.sortInPlace
            |> Js.Array.join

        let sorted = sort(s)
        List.map(sort, lst)
        |> List.map((==)(sorted))
        |> List.map2((x, y) => (x, y), lst)
        |> List.filter(((_, f)) => f)
        |> List.map(fst)
    }
}
