#!/usr/bin/env python3
import sys
import heapq


def solve(s: str, K: int):
    pq = []
    for i, c in enumerate(s):
        heapq.heappush(pq, (c, i))

    ret = set()
    while len(ret) < K:
        cs, i = heapq.heappop(pq)
        ret.add(cs)
        if i < (len(s) - 1):
            heapq.heappush(pq, (cs + s[i+1], i+1))
    ret = sorted(list(ret))
    print(ret[K-1])


# Generated by 1.1.4 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    s = next(tokens)  # type: str
    K = int(next(tokens))  # type: int
    solve(s, K)

if __name__ == '__main__':
    main()
