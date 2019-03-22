let abbreviate: string => string = s => s
|> Js.String.splitByRe([%re "/[\\s,-]/"])
|> Js.Array.filter((!=)(""))
|> Js.Array.map(x => x
    |> Js.String.charAt(0)
    |> Js.String.toUpperCase)
|> Js.Array.joinWith("")

