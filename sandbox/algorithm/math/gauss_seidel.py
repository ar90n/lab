import numpy as np

N = 32

# A must be positive semi-define symmetric matrix whose condition number is good
A = np.random.rand(N, N)
A = (A.T @ A)
D, U = np.linalg.eig(A)
A = (U.T @ np.diag(D + 1) @ U)
b = np.random.rand(N)

def gauss_seidel(A, b):
    D = np.diag(A)
    LU = A - np.diag(D)

    x = np.zeros_like(b)
    for _ in range(1024):
        last_x = x.copy()
        for i in range(N):
            x[i] = (b[i] - LU[i] @ x) / D[i]
        if np.average(np.abs(x - last_x)) < 1e-16:
            break
    return x

x = gauss_seidel(A, b)
err = np.average(np.abs(A @ x - b))
print(err)
