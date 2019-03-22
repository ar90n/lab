let isPangram: string => bool = s => {
    let s' = s
    |> String.lowercase
    |> Js.String.split("")
    |> Array.to_list
    |> List.filter(x => {
         let c = x |> Js.String.charCodeAt(0) |> int_of_float;
         (Char.code('a') <= c) && (c <= Char.code('z'))
       })
    |> Array.of_list
    |> Js.Array.joinWith("")

    Js.String.split("", s')
    |> Array.map(x => x |> Js.String.charCodeAt(0) |> int_of_float)
    |> Belt_SetInt.fromArray
    |> Belt_SetInt.size 
    |> (==)(26)
}
