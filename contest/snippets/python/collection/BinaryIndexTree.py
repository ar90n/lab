class BinaryIndexTree:
    def __init__(self, n):
        self._tree = [0] * (n + 1)

    def sum(self, i, j=None):
        if j is None:
            f = 0
            t = i
        else:
            f = i
            t = j

        return self._sum(t) - self._sum(f)

    def _sum(self, i):
        s = 0
        while 0 < i:
            s += self._tree[i - 1]
            i -= i & -i
        return s

    def add(self, i, x):
        i += 1
        while i < len(self._tree):
            self._tree[i - 1] += x
            i += i & -i
