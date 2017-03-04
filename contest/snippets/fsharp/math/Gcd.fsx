let rec Gcd a b =
    let (minv, maxv) = (min a b, max a b)
    if minv = 0 then maxv else Gcd (maxv % minv) minv

let Lcm a b =
    a * b / (Gcd a b)
