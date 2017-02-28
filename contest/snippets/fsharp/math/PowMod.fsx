let rec PowMod (a: int64) (b : int64) (c : int64) =
    if b = 0L then 1L
    else if ( b % 2L ) = 1L then ( a * ( PowMod a (b - 1L) c ) ) % c
    else 
        let d = PowMod a (b / 2L) c
        ( d * d ) % c
