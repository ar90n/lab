# %%
from typing import Callable, Any, Iterable
from sympy import lambdify, Matrix, MatrixSymbol, Eq, symbols, Inverse, diff
import numpy as np
import matplotlib.pyplot as plt


# %%
M = 5
x = MatrixSymbol("x", 2, 1)
Q = MatrixSymbol("Q", 2, 2)
c = MatrixSymbol("c", 2, 1)
u = MatrixSymbol("u", 1, 1)


# %%
def gradient(f, x):
    return Matrix([f]).jacobian(x).T


def hessian(f, x):
    return gradient(gradient(f, x), x)


# %%
f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
g_syms = [Eq(x[0] ** 2 - x[1], 0, evaluate=False)]

# %%
g_conds = lambdify(x, g_syms[0].lhs)
# %%
u = MatrixSymbol("u", 1, len(g_syms))
alpha = symbols("alpha")
rho = symbols("rho")
# x = MatrixSymbol("x", 2, 1)
B = MatrixSymbol("B", 2, 2)
s = MatrixSymbol("s", 2, 1)
y = MatrixSymbol("y", 2, 1)

L_sym = (
    f_sym
    + sum([ui * gi.lhs for ui, gi in zip(u, g_syms)])
    + rho / 2 * sum([gi.lhs**2 for gi in g_syms])
)
# %%
# %%
d_sym = -Inverse(B) * gradient(L_sym, x)
# %%
g_sym = L_sym.replace(x, x + alpha * d_sym).doit()
# %%
grad_L_sym = gradient(L_sym, x)

L = lambdify([rho, x], L_sym, "numpy")

g = lambdify([rho, x, u, B, alpha], g_sym, "numpy")
grad_g = lambdify([rho, x, u, B, alpha], diff(g_sym, alpha), "numpy")
grad_L = lambdify([rho, x, u], grad_L_sym, "numpy")

# %%
from quasi_newton_method import quansi_newton_method


def find_local_optima_by_quansi_newton_method(
    g,
    grad_g,
    grad_L,
    alpha,
    rho,
    u,
    x,
) -> np.ndarray:
    def cur_g(x, B, alpha):
        return g(rho, x, u, B, alpha)

    def cur_grad_g(x, B, alpha):
        return grad_g(rho, x, u, B, alpha)

    def cur_grad_L(x):
        return grad_L(rho, x, u)

    return quansi_newton_method(cur_g, cur_grad_g, cur_grad_L, x, alpha)[-1]


# %%
def augmented_lagrangian_method(
    g,
    grad_g,
    grad_L,
    initial_point,
    initial_alpha,
    initial_rho,
    criteria: float = 1e-4,
    max_iter: int = 4096,
):
    xs = [initial_point]
    cur_alpha = initial_alpha
    cur_rho = initial_rho
    cur_u = np.array([[0]])

    k = 0
    while criteria < abs(cur_rho * g_conds(xs[-1])):
        xs.append(
            find_local_optima_by_quansi_newton_method(
                g, grad_g, grad_L, cur_alpha, cur_rho, cur_u, xs[-1]
            )
        )
        cur_u = cur_u + cur_rho * g_conds(xs[-1])
        cur_rho *= 2

        k += 1
        if max_iter < k:
            raise ValueError("not converged")

    return xs


# %%
initial_point = np.array([[2, 1]]).T
initial_alpha = 1.0
initial_rho = 1.0

xs = augmented_lagrangian_method(
    g, grad_g, grad_L, initial_point, initial_alpha, initial_rho
)
# %%
xs
# %%
