#!/usr/bin/env python
# -*- coding:utf-8 -*-
import itertools

def main():
    res = []

    for v in itertools.permutations( "987654321", 9 ):
        for i in range( 1,9 ):
            wv = list( v[:] )
            c = int( str( ''.join( v[:i] ) ) )

            is_ok = True
            for j in range( 1, 10 ):
                d = str( c * j )
                if not ''.join(wv).startswith( d ):
                    is_ok = False
                    break
                wv = wv[len( d ):]
                if len( wv ) == 0:
                    break

            if is_ok:
                print( ''.join(v) )
                return

if __name__ == '__main__':
    main()
