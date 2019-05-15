class DGraph:
    def __init__(self, n, edges):
        self._vertice = [{} for _ in range(n)]
        for e in edges:
            self._vertice[e[0]][e[1]] = e[2] if len(e) == 3 else 1

    def __len__(self):
        return len(self._vertice)

    def edges(self, n=None):
        srcs = range(len(self)) if n is None else [n]
        return sum([[(s, d, c) for d, c in self._vertice[s].items()] for s in srcs], [])

    def transpose(self):
        tedges = [(t, f, c) for f, t, c in self.edges()]
        return DGraph(len(self), tedges)

    def cost(self, src, dst):
        return self._vertice[src].get(dst, float('inf'))

class Graph(DGraph):
    def __init__(self, n, edges):
        nd_edges = edges + [(d, s, c) for s, d, c in edges]
        super().__init__(n, nd_edges)

    def transpose(self):
        return self
