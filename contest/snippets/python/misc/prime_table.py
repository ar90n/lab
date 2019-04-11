def prime_table(n):
    t = [True] * (n + 1)
    t[0] = False
    t[1] = False

    i = 2
    while i * i <= n:
        for ii in range(2 * i, n + 1, i):
            t[ii] = False
        i += 1
    return [x for x, i in enumerate(t) if i == True]
