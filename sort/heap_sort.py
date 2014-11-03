#!/usr/bin/env python
# -*- coding:utf-8 -*-

import random
import sys

def heap_sort( data ):
    length = len( data )
    heap = []

    for d in data:
        heap.append( d )

        l = len( heap ) - 1
        while True:
            if l == 0:
                break

            parent = ( l - 1 ) // 2
            if heap[ l ] < heap[ parent ] :
                tmp = heap[ l ]
                heap[ l ] = heap[ parent ]
                heap[ parent ] = tmp

            l = parent

    for i in range( length ):
        v = heap[ 0 ]
        data[ i ] = v

        l = 0
        heap[ l ] = sys.maxsize
        while l <= length / 2:
            child1 = min( length - 1, l * 2 + 1 )
            child2 = min( length - 1, l * 2 + 2 )
            child = ( ( heap[ child1 ] < heap[ child2 ] ) and child1 ) or child2

            tmp = heap[ child ]
            heap[ child ] = heap[ l ]
            heap[ l ] = tmp

            l = child

def main():
    org_data = [ random.randint( 0, 10000 ) for i in range( 20 ) ]
    print( org_data )
    heap_sort( org_data )
    print( org_data )
    return

if __name__ == '__main__':
    main()
