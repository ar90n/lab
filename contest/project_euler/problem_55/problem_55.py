#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def rev( v ):
    return ''.join(list(reversed( str( v ) )))

def doit( v, n ):
    for n in range( 50 ):
        rv = int( rev( v ) )
        v = v + rv
        if str(v) == rev( v ):
            return 1

    return -1

def main():
    c = 0
    for i in range( 1, 10001 ):
        r = doit( i, 0 )
        if r == -1:
            c += 1

    print( c )
    return

if __name__ == '__main__':
    main()
