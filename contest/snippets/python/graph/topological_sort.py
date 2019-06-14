def topological_sort(g):
    result = []
    status = [0] * len(g)

    def _dfs(v):
        st = [v]
        while 0 < len(st):
            cv = st[-1]
            if status[cv] == 0:
                status[cv] = 1
                for _, d, _ in g.edges(cv):
                    if status[d] == 1:
                        raise ValueError('has cyclic')
                    st.append(d)
            elif status[cv] == 1:
                status[cv] = 2
                result.append(cv)
            if status[cv] == 2:
                st.pop()

    for v in range(len(g)):
        _dfs(v)
    return list(reversed(result))
