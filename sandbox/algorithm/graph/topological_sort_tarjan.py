from collections import deque

class Graph:
    def __init__(self, n: int, edges: list[(int, int)]):
        self.num = n
        self.edges = {i : [] for i in range(n)}
        for b,e in edges:
            self.edges[b].append(e)


def topo_sort(g: Graph):
    visited = [False]  * g.num
    def dfs(g: Graph, v: int) -> list[int]:
        if visited[v]:
            return []
        visited[v] = True

        ret = []
        for nv in g.edges[v]:
            if not visited[nv]:
                ret.append(dfs(g, nv))
        ret.append([v])
        ret.reverse()
        return sum(ret, [])

    ret = []
    for v in range(g.num):
        if not visited[v]:
            ret.append(dfs(g, v))
    ret.reverse()
    return sum(ret, [])

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