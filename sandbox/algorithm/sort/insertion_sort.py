#!/usr/bin/env python
# -*- coding:utf-8 -*-
import random

def insertion_sort( data ):
    length = len( data )

    for i in range( 1, length ):
        for j in range( i, 0, -1 ):
            if data[ j - 1] < data[ j ]:
                break;
            tmp = data[ j ]
            data[ j ] = data[ j - 1 ]
            data[ j - 1 ] = tmp
        print( data )
    pass

def main():
    org_data = [ random.randint( 0, 10000 ) for i in range( 20 ) ]
    print( org_data )
    insertion_sort( org_data )
    print( org_data )
    return

if __name__ == '__main__':
    main()
