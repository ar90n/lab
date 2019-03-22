let encode: string => string = s => {
    let aux = (acc,  s) => switch(acc){
        | [] => [(1, s)]
        | [(n, s'), ...tail] when s == s' => [(n + 1, s'), ...tail]
        | [(n, s'), ...tail] => [(1, s), (n, s'), ...tail]

    }
        
    Js.String.split("", s)
    |> Array.fold_left(aux, [])
    |> List.map(((n, s')) => (if(1 < n) string_of_int(n) else "") ++ s')
    |> List.rev
    |> Array.of_list
    |> Js.Array.joinWith("")
}

let decode: string => string = s => {
    let isDigit = c => (Char.code('0') <= Char.code(c)) && (Char.code(c) <= Char.code('9'))
    let isLower = c => (Char.code('a') <= Char.code(c)) && (Char.code(c) <= Char.code('z'))
    let isUpper = c => (Char.code('A') <= Char.code(c)) && (Char.code(c) <= Char.code('Z'))
    let isSpace = c => Char.code(' ') == Char.code(c)
    let isAlpha = c => isLower(c) || isUpper(c) || isSpace(c)

    let aux0 = (acc, c) => switch(acc) {
        | [] when isAlpha(c) => [[], [c]]
        | [] => [[c]]
        | [head, ...tail] when isAlpha(c) => [[], [c, ...head], ...tail]
        | [head, ...tail] => [[c, ...head], ...tail]
    }

    let rec powi = (x, n) => if(n == 0) 1 else x * powi(x, n -1)

    let aux2 = lst => lst
        |> List.mapi((i, c) => (powi(10, i) * (Char.code(c) - Char.code('0'))))
        |> List.fold_left((acc, x) => acc + x, 0)

    let aux1 = lst => switch(lst) {
        | [] => ""
        | [c] => String.make(1, c)
        | [c, ...ns] => String.make(aux2(ns), c)
    }

    Js.String.split("", s)
    |> Array.map(String.get(_, 0))
    |> Array.fold_left(aux0, [])
    |> List.rev
    |> List.map(aux1)
    |> Array.of_list
    |> Js.Array.joinWith("")
};
