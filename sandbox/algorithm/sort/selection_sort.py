#!/usr/bin/env python
# -*- coding:utf-8 -*-

import random

def selection_sort( data ):
    length = len( data )

    for i in range( length ):
        minimum_index = i
        for j in range( i, length ):
            if data[ j ] < data[ minimum_index ] :
                minimum_index = j
        tmp = data[ i ]
        data[ i ] = data[ minimum_index ]
        data[ minimum_index ] = tmp
    pass

def main():
    org_data = [ random.randint( 0, 10000 ) for i in range( 20 ) ]
    print( org_data )
    selection_sort( org_data )
    print( org_data )
    return

if __name__ == '__main__':
    main()
