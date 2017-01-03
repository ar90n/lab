#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import functools

def main():
    res = 0
    for a in range(99,0,-1):
        for b in range(99,0,-1):
            n = [ int(x) for x in list( str( a ** b ) ) ]
            m = functools.reduce( lambda x,y : x + y, n )
            if res < m :
                res = m
            if len( n ) < 99:
                print res
                return
    return

if __name__ == '__main__':
    main()

