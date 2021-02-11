from dataclasses import dataclass
from typing import Optional, Any

@dataclass
class Node:
    left: 'Option[Node]'
    right: 'Option[Node]'
    key: Any
    pri: float

def insert(t: Optional[Node], key: Any, pri:float) -> Node:
    if t is None:
        return Node(None, None, key, pri)
    if key < t.key:
        t.left = insert(t.left, key, pri)
        if t.pri < t.left.pri:
            t = rotate_right(t)
    else:
        t.right = insert(t.right, key, pri)
        if t.pri < t.right.pri:
            t = rotate_left(t)
    return t

def erase(t: Optional[Node], key: Any) -> Optional[Node]:
    if t is None:
        return None

    if key == t.key:
        if t.left is None and t.right is None:
            return None
        elif t.left is None:
            t = rotate_left(t)
        elif t.right is None:
            t = rotate_right(t)
        else:
            if t.left.pri < t.right.pri:
                t = rotate_left(t)
            else:
                t = rotate_right(t)
        t = erase(t, key)
    elif key < t.key:
        t.left = erase(t.left, key)
    else:
        t.right = erase(t.right, key)
    return t

def rotate_left(t: Node) -> Node:
    tmp = t.right
    t.right = tmp.left
    tmp.left = t
    return tmp

def rotate_right(t: Node) -> Node:
    tmp = t.left
    t.left = tmp.right
    tmp.right = t
    return tmp


def main():
    treap = None
    treap = insert(treap, 1, 100)
    treap = insert(treap, 2, 10)
    treap = insert(treap, 0, 99)
    treap = insert(treap, -1, 43)
    treap = insert(treap, 3, 43)
    treap = erase(treap, 1) 
    print(treap)


if __name__ == '__main__':
    main()
