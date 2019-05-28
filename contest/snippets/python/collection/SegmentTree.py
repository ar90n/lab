class SegmentTree:
    def __init__(self, n, f):
        self._size = n
        self._func = f

        n = 1
        while n < self._size:
            n *= 2
        self._tree = [None] * 2 * n

    def _filter(self, vl, vr):
        return list(filter(lambda x: x is not None, [vl, vr]))

    def update(self, i, x):
        i += (len(self._tree) // 2) - 1
        self._tree[i] = x

        while 0 < i:
            i = (i - 1) // 2
            values = self._filter(self._tree[2 * i + 1], self._tree[2 * i + 2])
            self._tree[i] = self._func(values) if 0 < len(values) else None

    def query(self, a, b):
        return self._query(a, b, 0, 0, len(self._tree) // 2)

    def _query(self, a, b, k, l, r):
        if r <= a or b <= l:
            return None

        if a <= l and r <= b:
            return self._tree[k]
        else:
            values = self._filter(
                self._query(a, b, 2 * k + 1, l, (l + r) // 2),
                self._query(a, b, 2 * k + 2, (l + r) // 2, r)
            )
            return self._func(values) if 0 < len(values) else None
