class UnionFind:
    def __init__(self, n):
        self._parents = list(range(n))
        self._ranks = [0] * n
        self._size = [1] * n

    def root(self, x):
        if self._parents[x] == x:
            return x
        self._parents[x] = self.root(self._parents[x])
        return self._parents[x]

    def size(self, x):
        return self._size[self.root(x)]

    def unite(self, x, y):
        rx = self.root(x)
        ry = self.root(y)
        if rx == ry:
            return

        if self._ranks[rx] < self._ranks[ry]:
            self._parents[rx] = ry
            self._size[ry] += self._size[rx]
        else:
            self._parents[ry] = rx
            self._size[rx] += self._size[ry]
            if self._ranks[rx] == self._ranks[ry]:
                self._ranks[rx] += 1

    def same(self, x, y):
        return self.root(x) == self.root(y)
