let validate: int => bool = n => {
    let w = float_of_int(n)
        |> log10
        |> floor
        |> int_of_float
        |> (+)(1)

    let rec aux = n => if (n == 0) [] else [(n mod 10), ...aux(n / 10)]
    let rec pow = (n, m) => if(m == 0) 1 else n * pow(n, m - 1)
    let an = aux(n)
    |> List.map(x => pow(x, w))
    |> List.fold_left((+), 0)
    an == n
}
