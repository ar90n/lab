#!/usr/bin/env python3
import sys
import heapq


def solve(N: int, M: int, A: "List[int]", B: "List[int]", C: "List[int]"):
    edges = sorted(list(zip(A, B, C)), key=lambda a: a[2])

    nodes = [i for i in range(N + 1)]
    s = [1 for _ in range(N + 1)]

    def root(x):
        while x != nodes[x]:
            x = nodes[x]
        return x
    
    def unite(x, y):
        x = root(x)
        y = root(y)
        if x == y:
            return
        
        if s[x] < s[y]:
            nodes[x] = y
            s[y] += s[x]
        else:
            nodes[y] = x
            s[x] += s[y]

    total = 0
    for a, b, c in edges:
        ra = root(a)
        rb = root(b)

        if ra != rb:
            unite(ra, rb)
            total += c
    print(total)
    return


# Generated by 2.13.0 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    A = [int()] * (M)  # type: "List[int]"
    B = [int()] * (M)  # type: "List[int]"
    C = [int()] * (M)  # type: "List[int]"
    for i in range(M):
        A[i] = int(next(tokens))
        B[i] = int(next(tokens))
        C[i] = int(next(tokens))
    solve(N, M, A, B, C)

if __name__ == '__main__':
    main()