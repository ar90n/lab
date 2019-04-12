import sys

b = sys.stdin.readline().strip()
r = {"A": "T", "T": "A", "C": "G", "G": "C"}[b]
print(r)
