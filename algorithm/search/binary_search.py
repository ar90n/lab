#!/usr/bin/env python
# -*- coding:utf-8 -*-

def binary_search( values, target ):
    left = 0
    right = len( values )

    while left < right:
        mid = ( left + right ) // 2

        if target == values[ mid ]:
            return True
        elif values[ mid ] < target:
            left = mid + 1
        else:
            right = mid

    return False

def main():
    data = sorted( [ 23,  43, 1, 33, 8 ] )
    print( binary_search( data, 43) )
    return

if __name__ == '__main__':
    main()
