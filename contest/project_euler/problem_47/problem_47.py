#!/usr/bin/env python3
# -*- coding: utf-8 -*-

primes = [ 2, 3, 5, 7, 11, 13 ]
mem = {}
def get_factors( n ):
    if mem.has_key(n):
        return mem[n]

    factors = 0
    nn = n
    for p in primes:
        if nn % p == 0:
            factors += 1
            while nn % p == 0:
                nn /= p

        if mem.has_key(nn):
            factors += mem[nn]
            nn = 1

    if factors == 0:
        primes.append( n )
        factors = 1

    mem[n] = factors
    return mem[n]

def main():
    i = 2
    w = 4
    factors = [0] * w
    while True:
        factors = factors[1:] + [get_factors(i)]
        if all( map( lambda x : x == w , factors ) ):
            print i - 3
            break
        i += 1
    return

if __name__ == '__main__':
    main()

