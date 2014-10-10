#!/usr/bin/env python
# -*- coding:utf-8 -*-


def main():
    ts = {}
    ps = {}
    hs = {}
    n = 144
    while True:
        t = ( n * ( n + 1 ) ) / 2
        p = ( n * ( 3 * n - 1 ) ) / 2
        h = n * ( 2 * n - 1 )
        ts[ t ] = n
        ps[ p ] = n
        hs[ h ] = n

        if ps.has_key( t ) and hs.has_key( t ):
            print( t, ps[ t ], hs[ t ], n )
            break

        n += 1

    return

if __name__ == '__main__':
    main()
