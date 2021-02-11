from operator import itemgetter
from collections import deque
import math

def arg(pt, orig) -> float:
    if pt == orig:
        return -math.inf

    dx = pt[0] - orig[0]
    dy = pt[1] - orig[1]
    return math.acos(-dy / math.sqrt(dx * dx + dy * dy))

def is_ccw(_next, cur, prev):
    dx0 = _next[0] - cur[0]
    dy0 = _next[1] - cur[1]
    dx1 = cur[0] - prev[0]
    dy1 = cur[1] - prev[1]

    return 0 < dx1 * dy0 - dy1 * dx0

def convex_hull(points: list[(int, int)]) -> list[(int, int)]:
    prev = min(points)
    points = sorted(points, key=lambda pt: arg(pt, prev))
    ret = points[:2]
    q = deque(points[2:])
    while 0 < len(q):
        pt = q.popleft()
        if is_ccw(pt, ret[-1], ret[-2]):
            ret.append(pt)
        else:
            ret.pop()
            q.appendleft(pt)
    ret.append(ret[0])
    return ret

def main():
    points = [(0, 0), (10, 10), (0, 10), (10, 0), (5, 5), (2, 2), (1, 3), (2, -20), (-1, 2)]
    ret = convex_hull(points)
    print(ret)

if __name__ == '__main__':
    main()