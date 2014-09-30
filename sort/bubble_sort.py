#!/usr/bin/env python
# -*- coding:utf-8 -*-

import random

def bubble_sort( data ):
    length = len( data )

    for i in range( 1, length ):
        for j in range( length - i ):
            if data[ j + 1 ] < data[ j ] :
                tmp = data[ j + 1 ]
                data[ j + 1 ] = data[ j ]
                data[ j ] = tmp

def main():
    org_data = [ random.randint( 0, 10000 ) for i in range( 20 ) ]
    print( org_data )
    bubble_sort( org_data )
    print( org_data )
    return

if __name__ == '__main__':
    main()
