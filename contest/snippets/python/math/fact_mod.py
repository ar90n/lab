def fact_mod(a, M):
    ret = 1
    for i in range(2, a + 1):
        ret = (ret * i) % M
    return ret
