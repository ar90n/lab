from collections import deque

class Graph:
    def __init__(self, n: int, edges: list[(int, int)]):
        self.num = n
        self.edges = {i : [] for i in range(n)}
        for b,e in edges:
            self.edges[b].append(e)


def topo_sort(g: Graph):
    in_degrees = [0] * g.num
    for _, ds in g.edges.items():
        for d in ds:
            in_degrees[d] += 1

    ret = []
    q = deque([i for i, d in enumerate(in_degrees) if d == 0])
    while 0 < len(q):
        ind = q.popleft()
        ret.append(ind)
        for n in g.edges[ind]:
            in_degrees[n] -= 1
            if in_degrees[n] == 0:
                q.append(n)
    return ret

def main():
    edges = [
        (0, 2),
        (0, 3),
        (1, 4),
        (2, 4),
        (3, 5),
        (4, 5)
    ]
    g = Graph(6, edges)
    order = topo_sort(g)
    print(order)

if __name__ == '__main__':
    main()