#!/usr/bin/env python3
import sys
from collections.abc import Iterable
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def make_exp_t(N, base):
    import cmath
    exp_t = {0: 1}
    temp = N
    while temp:
        exp_t[temp] = cmath.exp(base / temp)
        temp >>= 1
    return exp_t
 
 
def fft_dfs(f, s, N, st, exp_t):
    if N == 2:
        a = f[s]
        b = f[s+st]
        return [a+b, a-b]
    N2 = N//2
    st2 = st*2
    F0 = fft_dfs(f, s, N2, st2, exp_t)
    F1 = fft_dfs(f, s+st, N2, st2, exp_t)
    w = exp_t[N]
    wk = 1.0
    for k in range(N2):
        U = F0[k]
        V = wk * F1[k]
        F0[k] = U + V
        F1[k] = U - V
        wk *= w
    F0.extend(F1)
    return F0
 
 
def fft(a: list):
    import cmath
    n = len(a)
    N = 2 ** (2 * n - 1).bit_length()
    fft_exp_t = make_exp_t(N, -2j * cmath.pi)
    a.extend([0] * (N - n))
    if N == 1:
        return a
    return fft_dfs(a, 0, N, 1, fft_exp_t)
 
 
def ifft(a: list):
    import cmath
    N = len(a)
    ifft_exp_t = make_exp_t(N, 2j * cmath.pi)
    if N == 1:
        return a
    f = fft_dfs(a, 0, N, 1, ifft_exp_t)
    for i in range(N):
        f[i] /= N
    return f


def solve(N: int, A: "List[int]", B: "List[int]"):
    return [0] + [int(round(v.real)) for v in ifft([a * b for a, b in zip(fft(A), fft(B))])][:2 * N -1]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [int()] * (N)  # type: "List[int]" 
    B = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        A[i] = int(next(tokens))
        B[i] = int(next(tokens))
    result = solve(N, A, B)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
