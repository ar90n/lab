def binary_search(search_range, f, n=100, eps=1e-12):
    l, r = search_range
    c = (l + r) / 2.0
    for i in range(n):
        v = f(c)
        if abs(v) < eps:
            return c

        if v > 0:
            r = c
        else:
            l = c
        c = (l + r) / 2.0

    return c
