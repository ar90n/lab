#!/usr/bin/env python
# -*- coding:utf-8 -*-

import random

def quick_sort( data ):
    quick_sort_impl( data, 0, len( data ) - 1 )

def quick_sort_impl( data, l, r ):
    if ( r - l ) < 1:
        return

    li = l + 1
    ri = r
    while True:
        while True:
            if data[ l ] < data[ li ] or ri <= li:
                break
            li += 1
        while True:
            if data[ ri ] <= data[ l ] or ri <= li:
                break;
            ri -= 1

        if ri <= li:
            break

        tmp = data[ li ]
        data[ li ] = data[ ri ]
        data[ ri ] = tmp

    tmp = data[ li - 1 ]
    data[ li - 1 ] = data[ l ]
    data[ l ] = tmp

    quick_sort_impl( data, l, li - 1 )
    quick_sort_impl( data, ri, r )

def main():
    org_data = [ random.randint( 0, 10000 ) for i in range( 20 ) ]
    print( org_data )
    quick_sort( org_data )
    print( org_data )
    return

if __name__ == '__main__':
    main()
