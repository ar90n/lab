import sys

a = int(sys.stdin.readline())
b = int(sys.stdin.readline())
c = int(sys.stdin.readline())
d = int(sys.stdin.readline())
e = int(sys.stdin.readline())
k = int(sys.stdin.readline())
ret = ':(' if k < (e - a) else "Yay!"
print(ret)
