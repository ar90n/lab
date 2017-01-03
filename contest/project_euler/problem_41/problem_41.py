#!/usr/bin/env python
# -*- coding:utf-8 -*-
import itertools

def is_prime( vv ):
    i = 2;
    while i * i <= vv:
        if vv % i == 0:
            return False
        i += 1
    return True


def main():

    max_prime = 0
    for i in range( 9, 0, -1 ):
        for v in itertools.permutations( '123456789'[:i], i ):
            vv = int( ''.join( v ) )
            if is_prime( vv ) and max_prime < vv:
                max_prime = vv

    print( max_prime )
    return

if __name__ == '__main__':
    main()
