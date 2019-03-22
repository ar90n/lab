let wordCount: string => Js.Dict.t(int) = s => {
    let countUp = (acc, s) => {
        switch(acc) {
        | [(k, v), ...tail] when k == s => [(k, v + 1), ...tail]
        | _ => [(s, 1), ...acc]
        }
    }

    Js.String.toLowerCase(s)
    |> Js.String.splitByRe([%re "/'?[^A-Za-z0-9']+'?/"])
    |> Array.to_list
    |> List.filter((x) => x != "")
    |> List.sort((x,y) => Js.String.localeCompare(x,y) |> int_of_float)
    |> List.fold_left(countUp, [])
    |> Js.Dict.fromList
}
