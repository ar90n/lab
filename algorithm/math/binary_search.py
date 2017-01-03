#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import math

def binary_search( l, r, k, f, n = 100, eps = 1e-12 ):
    c = ( l + r ) / 2.0 
    for i in range( n ):
        v = f( c )
        if abs( v - k ) < eps:
            return c

        if v > k :
            r = c
        else:
            l = c
        c = ( l + r ) / 2.0 

    return c

def main():
    l = 0
    r = 10
    k = 3.1
    f = lambda x : math.sqrt( x )
    print( binary_search( l, r, k, f ) )
    return

if __name__ == '__main__':
    main()

