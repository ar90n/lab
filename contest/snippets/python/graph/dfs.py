def dfs(g, src, dst):
    visited = set([])
    vertex_stack = [src]
    while vertex_stack:
        current_vertex = vertex_stack.pop()
        if current_vertex == dst:
            return True

        if current_vertex in visited:
            continue
        visited.add(current_vertex)

        for next_vertex in g.setdefault(current_vertex, {}).keys():
            vertex_stack.append(next_vertex)

    return False
