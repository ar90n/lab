#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools

def get_prime_table( n ):
    t = [ True ] * ( n + 1 )
    t[ 0 ] = False
    t[ 1 ] = False

    i = 2
    while i * i <= n:
        for ii in range( 2 * i, n + 1, i ):
            t[ ii ] = False
        i += 1
    return [ x for x,i in enumerate( t ) if i == True ]

def main():
    pt = get_prime_table( 1000000 )
    acc_pt = [0] + list( itertools.accumulate( pt ) )
    set_pt = set( pt )

    res = []
    for w in range( 2, len( acc_pt ) ):
        i = 0
        while i + w < len( acc_pt ):
            p = acc_pt[ i + w ] -  acc_pt[ i ]
            if pt[-1] < p:
                break

            if p in set_pt:
                res.append( ( w, p ) )
            i += 1
    print( res[ -1 ] )
    return

if __name__ == '__main__':
    main()
