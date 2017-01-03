#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools
import collections

def is_prime( p ):
    if p < 2:
        return False

    i = 2;
    while i * i <= p:
        if p % i == 0:
            return False
        i += 1
    return True

def main():
    t = range( 10 )
    for c1 in t:
        for c2 in t[ c1: ]:
            for c3 in t[ c2: ]:
                for c4 in t[ c3: ]:
                    c = ''.join( [ str( x ) for x in [ c1, c2, c3, c4 ] ] )

                    res = []
                    for p in itertools.permutations( c, 4 ):
                        prime = int( ''.join( p ) )
                        if prime < 1000:
                            continue

                        if is_prime( prime ):
                            res.append( prime )

                    if 3 <= len( res ):
                        dic = collections.defaultdict( set )
                        for i,x in enumerate( res ):
                            for y in res[ i + 1: ]:
                                diff = abs( y - x )
                                dic[ diff ].add( (x,y) )
                        for diff, vs in  dic.items():
                            if len( vs ) == 2:
                                x1, x2 = vs.pop()
                                x3, x4 = vs.pop()
                                xx = sorted( [ x1, x2, x3, x4 ] )
                                if xx[ 1 ] == xx[ 2 ]:
                                    print( diff, str( xx[ 0 ] ) + str( xx[ 1 ] ) + str( xx[ 3 ] ) )

    return

if __name__ == '__main__':
    main()
