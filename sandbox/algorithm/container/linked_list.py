class Node:
    def __init__(self, value, next):
        self.value = value
        self.next = next

class LinkedList:

    def __init__(self):
        self._head = None
        self.count = 0

    def insert(self, value, index=None):
        if self._head is None:
            self._head = Node(value, None)
        else:
            index = (self.count - 1) if index is None else index
            cur = self._get(index)
            new = Node(value, cur.next)
            cur.next = new
        self.count += 1

    def delete(self, value):
        cur = self._head
        if cur.value == value:
            self._head = cur.next
        else:
            while cur.next is not None:
                if cur.next.value == value:
                    break
                cur = cur.next
            cur.next = cur.next.next
        self.count -= 1

    def get(self, index):
        return self._get(index).value

    def _get(self, index):
        if self.count <= index or index < 0:
            raise ValueError('given illegal index')

        cur_index = 0
        cur = self._head
        while (cur_index != index):
            cur = cur.next
            cur_index += 1
        return cur



if __name__ == '__main__':
    l = LinkedList()
    assert l.count == 0

    l.insert(10)
    l.insert(11)
    l.insert(9)
    assert l.count == 3
    assert l.get(0) == 10
    assert l.get(1) == 11
    assert l.get(2) == 9

    l.delete(11)
    assert l.count == 2
    assert l.get(0) == 10
    assert l.get(1) == 9

    l.delete(10)
    assert l.count == 1
    assert l.get(0) == 9

    try:
        l.get(1)
    except ValueError as e:
        assert str(e) == 'given illegal index'
    try:
        l.get(-1)
    except ValueError as e:
        assert str(e) == 'given illegal index'

    l.delete(9)
    assert l.count == 0
    try:
        l.get(0)
    except ValueError as e:
        assert str(e) == 'given illegal index'
