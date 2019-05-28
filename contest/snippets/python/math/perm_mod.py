def perm_mod(n, m, M):
    ret = 1
    for i in range(n, n - m, -1):
        ret = (ret * i) % M
    return ret
