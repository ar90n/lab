#!/usr/bin/env python
# -*- coding:utf-8 -*-

import math

def main():
    res = 0
    for n in range( 1, 101 ):
        for r in range( 1, n + 1 ):
            v1 = math.factorial( n )
            v2 = math.factorial( r )
            v3 = math.factorial( n - r )
            v = v1 / ( v2 * v3 )
            if 1000000 < v:
                res += 1
    print( res )
    return

if __name__ == '__main__':
    main()
