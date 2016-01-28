#!/usr/bin/env python
# -*- coding:utf-8 -*-

import math

def is_prime( n, primes ):
    is_prime = True
    for p in primes:
        if n % p == 0:
            is_prime = False
            break
    return is_prime

def main():
    memo = {}
    primes = [2, 3, 5, 7]
    n = 9
    while True:
        is_ok = False
        try:
            if is_prime( n, primes ):
                primes.append( n )

            for p in primes:
                for i in range(0,int(math.sqrt(n))):
                    diff = n - p - 2 * i * i
                    if diff == 0:
                        is_ok = True
                        raise Exception
        except Exception:
            pass

        if not is_ok:
            print n
            break
        n += 2

if __name__ == '__main__':
    main()
