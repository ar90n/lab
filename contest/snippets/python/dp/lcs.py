def lcs(xs, ys):
    dp = [([0] * (len(ys) + 1)) for _ in range(len(xs) + 1)]
    for i, x in enumerate(xs):
        for j, y in enumerate(ys):
            dp[i + 1][j + 1] = max(dp[i][j + 1], dp[i + 1][j], dp[i][j] + int(x == y))
    return dp[-1][-1]
