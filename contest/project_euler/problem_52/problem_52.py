#!/usr/bin/env python
# -*- coding:utf-8 -*-

import itertools

def main():
    for l in range( 1, 10 ):
        for tv in itertools.permutations( "023456789", l ):
            v = "1" + ''.join( tv )
            #print( l, v )
            vi = int( v )
            vss = [ set( str( x * vi ) ) for x in range( 1, 7 ) ]
            #print( vss )

            is_ok = True
            for vs in vss:
                if vss[ 0 ] != vs:
                    is_ok = False
                    break
            if is_ok:
                print( v )
                return
    return

if __name__ == '__main__':
    main()
