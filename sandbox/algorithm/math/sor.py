import numpy as np

N = 1024

A = np.random.rand(N, N) + N * N * np.eye(N)
A = (A.T @ A)
b = np.random.rand(N)
w = 1.5

def gauss_seidel(A, b):
    D = np.diag(A)
    LU = A - np.diag(D)

    x = np.zeros_like(b).astype(np.float64)
    for _ in range(128):
        last_x = x.copy()
        for i in range(N):
            x[i] = (1 - w) * x[i] + w * (b[i] - LU[i] @ x) / D[i]
        if np.average(np.abs(x - last_x)) < 1e-16:
            break
    return x

x = gauss_seidel(A, b)
err = np.average(np.abs(A @ x - b))
print(err)
