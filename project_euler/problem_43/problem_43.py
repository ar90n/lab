#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools

def main():

    res = 0
    t = '0123456789'
    for h in range( 1, 10 ):
        for tv in itertools.permutations( t[:h] + t[h+1:], 9 ):
            v = str( h ) + ''.join( tv )
            if len( v ) != 10:
                continue

            is_ok = True
            for p,i in zip( [ 2,3,5,7,11,13,17 ], range( 1, 8 ) ):
                vv = int( v[i:i+3] )
                if vv % p != 0:
                    is_ok = False
                    break

            if is_ok:
                res += int( v )

    print(res)
    return

if __name__ == '__main__':
    main()
