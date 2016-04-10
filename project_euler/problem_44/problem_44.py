#!/usr/bin/env python
# -*- coding:utf-8 -*-

def main():
    memo = {}
    n = 1
    while True:
        s = n * ( 3 * n - 1 ) / 2
        memo[s] = 1
        for i in range(1,n):
            pi = i * ( 3 * i - 1 ) / 2
            pj = s - pi
            if memo.has_key( pj ):
                D = abs( pi - pj )
                if memo.has_key( D ):
                    print D
                    return
        n += 1

if __name__ == '__main__':
    main()
