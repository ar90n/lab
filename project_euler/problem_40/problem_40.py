#!/usr/bin/env python
# -*- coding:utf-8 -*-
#('1', '1', '5', '3', '7')
def getNum( num ):
    for w in [ 0, 10, 100, 1000, 10000, 100000, 1000000 ]:
        l = len( str( w ) )
        v = 9 * l * ( 10 ** ( l - 1 ) )
        if num <= v:
            q = ( num // l )
            r = ( num % l ) - 1
            n = w + q
            res = int(str( n )[ r ])
            return res
        num -= v

def main():
    res = 1
    for x in [ 1, 10, 100, 1000, 10000, 100000, 1000000 ]:
        res *= getNum( x )
    print( res )

    return

if __name__ == '__main__':
    main()
