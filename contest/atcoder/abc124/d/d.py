import sys
import numpy as np
import heapq

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]


n, k = reatIntN()
ss = sys.stdin.readline().strip()


class Node:
    def __init__(self, p, n, v):
        self.prev = p
        self.next = n
        self.value = v


r = Node(None, None, None)
r.next = r
r.prev = r

def append(n, value):
    nn = Node(n, n.next, value)
    n.next.prev = nn
    n.next = nn

def remove(n):
    pn = n.prev
    nn = n.next
    nn.prev = pn
    pn.next = nn
    n.next = None
    n.prev = None
    n.value = None


i = 0
while i < len(ss):
    b = i
    c = ss[i]
    while (i < len(ss)) and (c == ss[i]):
        i += 1
    append(r, (int(c), b, i))


def _calc(p):
    d = 0
    if (p is not r) and (p.value[0] == 0):
        lh = p.value[1]
        rh = p.value[2]
        if p.next is not r:
            lh = min(lh, p.next.value[1])
        if p.prev is not r:
            rh = max(rh, p.prev.value[2])
        d = rh - lh
    return d


p = r.next
pq = []
while p is not r:
    d = _calc(p)
    if 0 < d:
        heapq.heappush(pq, (-d, p.value[2], p))
    p = p.next


for i in range(k):
    if len(pq) == 0:
        break
    _, _, p = heapq.heappop(pq)
    if (p.value is not None) and (p.value[0] == 0):
        lh = p.value[1]
        rh = p.value[2]

        if p.next is not r:
            lh = min(lh, p.next.value[1])
            remove(p.next)
        if p.prev is not r:
            rh = max(rh, p.prev.value[2])
            remove(p.prev)
        p.value = (1, lh, rh)

        dp = _calc(p.prev)
        if 0 <= dp:
            heapq.heappush(pq, (-dp, p.prev.value[2], p.prev))
        dn = _calc(p.next)
        if 0 <= dn:
            heapq.heappush(pq, (-dn, p.next.value[2], p.next))

ret = 0
p = r.next
while p is not r:
    if p.value[0] == 1:
        ret = max(ret, p.value[2] - p.value[1])
    p = p.next
print(ret)
