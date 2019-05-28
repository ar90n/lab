def prime_table(n):
    t = [True] * (n + 1)
    t[0] = False
    t[1] = False

    for p in range(2, n + 1, 2):
        if n < p ** 2:
            break
        if t[p]:
            for i in range(p * p, n + 1, 2 * p):
                t[i] = False
    return [2] + [p for p in range(3, n + 1, 2) if t[p]]
