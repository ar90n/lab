def lcs(xs, ys):
    dp = [([0] * (len(ys) + 1)) for _ in range(len(xs) + 1)]
    for i, x in enumerate(xs):
        for j, y in enumerate(ys):
            dp[i + 1][j + 1] = max(dp[i][j + 1], dp[i + 1][j], dp[i][j] + int(x == y))

    s = []
    p = (-1, -1)
    while 0 < dp[p[0]][p[1]]:
        if dp[p[0]][p[1]] == dp[p[0]-1][p[1]]:
            p = (p[0]-1, p[1])
        elif dp[p[0]][p[1]] == (dp[p[0]-1][p[1]-1] + int(xs[p[0]] == ys[p[1]])):
            s.append(xs[p[0]])
            p = (p[0]-1, p[1]-1)
        else:
            p = (p[0], p[1]-1)

    return ''.join(reversed(s))
