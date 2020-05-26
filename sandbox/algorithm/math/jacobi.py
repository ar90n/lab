import numpy as np

N = 4096

# A must be diagonal dominat
A = np.random.rand(N, N) + N * N * np.eye(N)
A = (A + A.T) / 2
b = np.random.rand(N)

def jacobi(A, b):
    D = np.diag(A)
    LU = A - np.diag(D)
    invD = 1.0 / D

    x = np.zeros_like(b)
    for _ in range(1024):
        nx = invD * (b - LU @ x)
        if np.average(np.abs(nx - x)) < 1e-16:
            break
        x = nx
    return x

x = jacobi(A, b)
err = np.average(np.abs(A @ x - b))
print(err)
