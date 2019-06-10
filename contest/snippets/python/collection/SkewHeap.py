class SkewHeap:
    def __init__(self):
        self._root = None
        self._size = 0

    def append(self, v):
        self._root = self._meld(self._root, (v, None, None))
        self._size += 1

    def popleft(self):
        if self._root is None:
            return None

        v, lh, rh = self._root
        self._root = self._meld(lh, rh)
        self._size -= 1
        return v

    def _meld(self, a, b):
        if a is None:
            return b
        if b is None:
            return a
        a, b = (a, b) if b[0] < a[0] else (b, a)
        return (a[0], self._meld(a[2], b), a[1])

    def __len__(self):
        return self._size
