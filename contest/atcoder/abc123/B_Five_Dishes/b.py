import sys
c = [int(sys.stdin.readline()) for _ in range(5)]
cc = [(v + 9) // 10 * 10 for v in c]
ccc = sorted(zip(cc, c), key=lambda x:x[1] - x[0], reverse=True)
ret = sum([v[0] for v in ccc[:-1]]) + ccc[-1][1]
print(ret)
