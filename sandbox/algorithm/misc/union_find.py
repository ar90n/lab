#!/usr/bin/env python
# -*- coding:utf-8 -*-

table = {}
def unify( a, b ):
    table[ a ] = b

def find( a ):
    while a in table:
        a = table[ a ]
    return a

def main():
    unify( 'a', 'b' )
    unify( 'b', 'd' )
    unify( 'c', 'e' )
    print( find( 'a' ) == find( 'd' ) )
    print( find( 'b' ) == find( 'c' ) )
    return

if __name__ == '__main__':
    main()
