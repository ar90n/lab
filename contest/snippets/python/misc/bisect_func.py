def bisect_func(search_range, f):
    lo, hi = search_range
    while (lo+1) < hi:
        mid= (lo + hi) // 2
        if 0 <= f(mid):
            hi = mid
        else:
            lo = mid
    return lo + 1
