#!/usr/bin/env python
# -*- coding:utf-8 -*-

def gcd1( a, b ):
    a, b = ( ( a > b ) and ( a, b ) ) or ( b, a )
    if b == 0:
        return a
    return gcd1( b, a % b )

def gcd2( a, b ):
    a, b = ( ( a > b ) and ( a, b ) ) or ( b, a )
    while b:
        a, b = b, a % b
    return a
def main():
    print( gcd1( 20, 16 ) )
    print( gcd1( 1071, 1029 ) )
    print( gcd2( 1071, 1029 ) )
    print( gcd1( 1029, 1071 ) )
    print( gcd2( 1029, 1071 ) )
    return

if __name__ == '__main__':
    main()
