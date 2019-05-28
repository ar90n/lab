def comb_mod(n, m, M):
    return (perm_mod(n, m, M) * inv_mod(fact_mod(m, M), M)) % M
