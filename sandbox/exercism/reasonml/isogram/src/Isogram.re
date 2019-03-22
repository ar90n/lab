let isLower = s => {
    let c = s
    |> Js.String.charCodeAt(0)
    |> int_of_float;
    (Char.code('a') <= c) && (c <= Char.code('z'))
}

let isUpper = s => {
    let c = s
    |> Js.String.charCodeAt(0)
    |> int_of_float;
    (Char.code('A') <= c) && (c <= Char.code('Z'))
}

let isAlpha = s => isLower(s) || isUpper(s)

let is_isogram: string => bool = s => {
let s' = s
|> String.lowercase
|> Js.String.split("")
|> Array.to_list
|> List.filter(isAlpha)
|> Array.of_list
|> Js.Array.joinWith("")

s'
|> Js.String.split("")
|> Array.map(s => s
    |> Js.String.charCodeAt(0)
    |> int_of_float
   )
|> Belt_SetInt.fromArray
|> Belt_SetInt.size
|> (==)(String.length(s'))
}
