from operator import itemgetter
import math

def arg(_next, cur, prev) -> float:
    if _next == cur:
        return math.inf

    dx0 = _next[0] - cur[0]
    dy0 = _next[1] - cur[1]
    dx1 = cur[0] - prev[0]
    dy1 = cur[1] - prev[1]
    num = dx0 * dx1 + dy0 * dy1
    den = math.sqrt((dx0 * dx0 + dy0 * dy0)* (dx1 * dx1 + dy1 * dy1))
    return math.acos(num / den)

def convex_hull(points: list[(int, int)]) -> list[(int, int)]:
    prev = min(points, key=itemgetter(0))
    cur = min(points, key=lambda pt: arg(pt, prev, (prev[0], prev[1] - 1)))
    ret = [prev, cur]
    while ret[0] != ret[-1]:
        n = min(points, key=lambda pt: arg(pt, ret[-1], ret[-2]))
        ret.append(n)
    return ret

def main():
    points = [(0, 0), (10, 10), (0, 10), (10, 0), (5, 5), (2, 2), (1, 3), (2, -20), (-1, 2)]
    ret = convex_hull(points)
    print(ret)

if __name__ == '__main__':
    main()