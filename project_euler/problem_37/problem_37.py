#!/usr/bin/env python
# -*- coding:utf-8 -*-

primes = set( [] )
def gen_primes():
    work = [ True ] * 1000000

    work[ 0 ] = False
    work[ 1 ] = False
    for i in range( len( work ) ):
        if not work[ i ] :
            continue
        for j in range( 2 * i, len( work ), i ):
            work[ j ] = False

    for i in range( len( work ) ):
        if work[ i ] :
            primes.add( i )

def main():
    gen_primes()

    res = []
    for prime in  primes:
        is_ok = True

        lp = list( str( prime ) )
        for i in range( 1, len( lp ) ):
            v1 = int( ''.join( lp[i:] ) )
            v2 = int( ''.join( lp[:-i] ) )
            if v1 not in primes or v2 not in primes:
                is_ok = False
                break
        if is_ok:
            res.append( prime )
    print( sum( res[4:] ) )
    return

if __name__ == '__main__':
    main()
