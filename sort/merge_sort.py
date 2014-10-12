#!/usr/bin/env python
# -*- coding:utf-8 -*-

import random

def merge_sort( data ):
    l = len( data )
    if l <= 1:
        return data

    left_data = merge_sort( data[:(l//2)] )
    right_data = merge_sort( data[(l//2):] )

    res = []
    li = 0
    ri = 0
    while left_data[li:] and right_data[ri:]:
        if left_data[ li ] <= right_data[ ri ]:
            res.append( left_data[ li ] )
            li += 1
        else:
            res.append( right_data[ ri ] )
            ri += 1
    res += left_data[li:]
    res += right_data[ri:]

    return res

def main():
    org_data = [ random.randint( 0, 10000 ) for i in range( 20 ) ]
    print( org_data )
    res_data = merge_sort( org_data )
    print( res_data )
    return

if __name__ == '__main__':
    main()
