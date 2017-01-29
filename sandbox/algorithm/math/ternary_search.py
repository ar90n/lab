#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import math

def ternary_search( l, r, f, n = 100, eps = 1e-24 ):
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

def main():
    l = 0
    r = 10
    f = lambda x : -math.pow( ( x - 2.4 ), 2 )
    print( ternary_search( l, r, f ) )
    return

if __name__ == '__main__':
    main()

