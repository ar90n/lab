def ternary_search( search_range, f, n = 100, eps = 1e-24 ):
    l, r = search_range
    for i in range( n ):
        ll = ( l + l + r ) / 3.0
        rr = ( l + r + r ) / 3.0

        vl = f( ll )
        vr = f( rr )
        if abs( vl - vr ) < eps:
            break

        if vl > vr :
            r =rr 
        else:
            l =ll 

    return ( l + r ) / 2.0
