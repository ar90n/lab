#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
try:
    from math import gcd
except Exception:
    from fractions import gcd

YES = "Yes"  # type: str
NO = "No"  # type: str

def solve(S: str, T: str):
    SS = S + S
    for i in range(len(S)):
        if SS[i:i+len(S)] == T:
            return YES
    return NO


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = next(tokens)  # type: str
    T = next(tokens)  # type: str
    result = solve(S, T)
    print(result)

if __name__ == '__main__':
    main()
