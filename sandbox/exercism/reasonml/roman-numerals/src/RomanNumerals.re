let toRoman: int => string = x => {
    let aux = (n, a, b, c) => {
        switch(n) {
        | _ when n == 0 => ""
        | _ when n < 4 => String.make(n, a)
        | _ when n == 4 => String.make(1, a) ++ String.make(1, b)
        | _ when n < 9 => String.make(1, b) ++  String.make((n - 5), a)
        | _ => String.make(1, a) ++ String.make(1, c)
        }
    }

    aux(((x / 1000) mod 10), 'M', 'M', 'M') ++
    aux(((x / 100) mod 10), 'C', 'D', 'M') ++
    aux(((x / 10) mod 10), 'X', 'L', 'C') ++
    aux((x mod 10), 'I', 'V', 'X')
}
