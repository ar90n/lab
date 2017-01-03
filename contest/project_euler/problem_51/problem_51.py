#!/usr/bin/env python
from itertools import *
from bisect import bisect_left

def primes(n):
    s=range(0,n+1)
    s[1]=0
    bottom=2
    top=n//bottom
    while (bottom*bottom<=n):
            while (bottom<=top):
                    if s[top]:
                            s[top*bottom]=0
                    top-=1
            bottom+=1
            top=n//bottom
    return [x for x in s if x]

def is_prime(n, primes_tbl):
    i = bisect_left(primes_tbl, n)
    return i != len(primes_tbl) and primes_tbl[i] == n

def number_families(num):
    digits = [d for d in str(num)]
    products = list(product((True, False), repeat=len(digits)))[1:-1]
    for p in products:
        pattern = ''
        for i, x in enumerate(p):
            if x:
                pattern += digits[i]
            else:
                pattern += 'X'
        yield [int(pattern.replace('X', str(n))) for n in range(10)]

def main():
    primes_tbl = primes(1000000)
    for prime in primes_tbl:
        for number_family in number_families(prime):
            prime_family = [n for n in number_family if is_prime(n, primes_tbl) and len(str(n)) == len(str(prime))]
            if len(prime_family) == 8 and prime in prime_family:
                print(prime)
                return

if __name__ == "__main__":
    main()
