class Node:
    def __init__(self, value, left, right):
        self.value = value
        self.left = left
        self.right = right

class BinaryTree:
    def __init__(self):
        self._root = None

    def _find_parent(self, cur, value):
        if cur is None or cur.value == value:
            return None

        next_side = cur.left if value < cur.value else cur.right
        node = self._find_parent(next_side, value)
        if node is None:
            return cur
        return node

    def _find(self, cur, value):
        if cur is None:
            return None

        print(cur.value)
        if cur.value == value:
            return cur
        elif cur.value < value:
             return self._find(cur.right, value)
        elif value < cur.value:
            return self._find(cur.left, value)

    def find(self, value):
        cur = self._find(self._root, value)
        return cur if cur is None else cur.value

    def add(self, value):
        if self._root is None:
            self._root = Node(value, None, None)
        else:
            p = self._find_parent(self._root, value)
            if p.value < value and p.right is None:
                p.right = Node(value, None, None)
            elif value < p.value and p.left is None:
                p.left = Node(value, None, None)

    def delete(self, value):
        if self._root is None:
            return

        p = self._find_parent(self._root, value)
        if p is None:
            if self._root.left is not None  and self._root.right is not None:
                cur = self._root.left
                prev = None
                while cur.right is not None:
                    prev = cur
                    cur = cur.right
                if prev is not None:
                    prev.right = None
                print('-----')
                print(self._root.left.value, self._root.left.value)
                print(self._root.value, cur.value)
                print(self._root.left.left.value, cur.value)
                print('-----')
                self._root.value = cur.value
            elif self._root.left is None:
                self._root = self._root.right
            else:
                self._root = self._root.left
            return

        is_left_child = True
        if p.left is not None and p.left.value == value:
            c = p.left
        elif p.right is not None and p.right.value == value:
            c = p.right
            is_left_child = False
        else:
            return

        if c.left is not None and c.right is not None:
            cur = c.left
            prev = None
            while cur.right is not None:
                prev = cur
                cur = cur.right
            if prev is not None:
                prev.right = None
            c.value = cur.value
        elif c.left is None:
            if is_left_child:
                p.left = cur.right
            else:
                p.right = cur.right
        else:
            if is_left_child:
                p.left = cur.left
            else:
                p.rifht = cur.left


if __name__ == '__main__':
    bt = BinaryTree()
    assert bt.find(0) is None

    bt.add(10)
    bt.add(12)
    bt.add(11)
    bt.add(13)
    bt.add(8)
    bt.add(7)
    bt.add(9)
    bt.add(9)
    assert bt.find(10) == 10
    assert bt.find(11) == 11
    assert bt.find(9) == 9
    assert bt.find(8) == 8
    assert bt.find(15) is None

    bt.delete(8)
    assert bt.find(8) is None
    bt.delete(8)
    assert bt.find(8) is None

    bt.delete(12)
    assert bt.find(12) is None

    bt.add(8)
    assert bt.find(10) == 10
    assert bt.find(11) == 11
    assert bt.find(9) == 9
    assert bt.find(8) == 8

    bt.delete(10)
    assert bt.find(10) is None

    assert bt.find(11) == 11
    assert bt.find(9) == 9
    print('----')
    assert bt.find(8) == 8
