#!/usr/bin/env python
# -*- coding:utf-8 -*-

import math

def main():
    max_res = 0
    for p in range( 1000, 0, -1 ):
        res = 0
        for c in range( 1, p // 2 ):
            e = p - c
            d = 2 * c * c - e * e
            if d <= 0 :
                continue

            f = int( math.sqrt( d ) )
            if d != f * f :
                continue

            if ( ( e + f ) % 2 != 0 ) or ( ( e - f ) % 2 != 0 ):
                continue

            res += 1
        if max_res < res:
            max_res = res
    print( max_res )
    return

if __name__ == '__main__':
    main()
