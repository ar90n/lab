def pow_mod(a, k, M):
    if k == 0:
        return 1

    t = pow_mod(a, k // 2, M)
    res = (t * t) % M
    if k % 2 == 1:
        res = (res * a) % M

    return res
