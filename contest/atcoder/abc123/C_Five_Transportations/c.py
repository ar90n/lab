import sys
import math

n = int(sys.stdin.readline())
cs = [int(sys.stdin.readline()) for _ in range(5)]
mc = min(cs)
ret = 4 + int(math.ceil(n / mc))
print(ret)
