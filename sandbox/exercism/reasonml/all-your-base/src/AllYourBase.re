let rebase: (int, list(int), int) => option(list(int)) = (a, lst, b) => {
    let rec powi = (a, b) => if(b ==  0) 1 else a * powi(a, b - 1)

    let f = (x, y, l) => {
        let aux: (option(list(int)), int) => option(list(int)) = (ret, x') => switch(ret) {
        | None => None
        | _ when x' < 0 => None
        | _ when (x - 1) < x' => None
        | Some(l') => Some([x', ...l'])
       }

        let init = if(x <= 0 || y <= 0) None else Some([])
        List.fold_left(aux, init, l)
    }

    let d = (x, v) => switch(v) {
    | None => None
    | Some(l) => Some(l |> List.mapi((i, v) => v * powi(x, i)) |> List.fold_left((+), 0))
    }

    let e = (x, v) => {
        let rec aux = (v') => if(v' < x) [v'] else [(v' mod x), ...aux(v' / x)]
        switch(v) {
        | None => None
        | Some(v') when v' == 0 => None
        | Some(v') => Some(aux(v') |> List.rev)
        }
    }
    f(a, b, lst) |> d(a) |> e(b)
}
