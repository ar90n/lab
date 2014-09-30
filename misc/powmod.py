#!/usr/bin/env python
# -*- coding:utf-8 -*-

def powmod( a, k, m ):
    if k == 0:
        return 1

    t = powmod( a, k // 2, m )
    res = ( t * t ) % m
    if k % 2 == 1:
        res = ( res * a ) % m

    return res

def main():
    print( powmod( 3, (( 1 << 31) -1 ) , 10000 ) )
    return

if __name__ == '__main__':
    main()
