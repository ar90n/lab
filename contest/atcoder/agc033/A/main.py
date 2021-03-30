#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *


def solve(H: int, W: int, A: "List[str]"):
    queue = deque()

    rem = H * W
    visited = [True] * ((W+2) * (H+2))
    for i in range(W+2):
        visited[i] = False
        visited[(W + 2) * (H + 1) + i] = False
    for i in range(H+2):
        visited[(W + 2) * i] = False
        visited[(W + 2) * i + (W + 1)] = False

    for i in range(H):
        l = A[i]
        for j in range(W):
            if l[j] == '#':
                queue.append((i+1, j+1, 0))
                k = i * (W + 2) + j
                visited[k] = False
    
    ret = 0
    while queue:
        i, j, n = queue.popleft()
        k = i * (W + 2) + j
        if not visited[k] and 0 < n:
            continue
        visited[k] = False
        ret = n
        rem -= 1

        if rem == 0:
            break

        if visited[k - 1]:
            queue.append((i, j - 1, n + 1))
        if visited[k + 1]:
            queue.append((i, j + 1, n + 1))

        if visited[k - (W + 2)]:
            queue.append((i - 1, j, n + 1))
        if visited[k + (W + 2)]:
            queue.append((i + 1, j, n + 1))
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    A = [ next(tokens) for _ in range(H) ]  # type: "List[str]"
    result = solve(H, W, A)
    print(result)

if __name__ == '__main__':
    main()
