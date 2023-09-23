# %%
from typing import Any, Callable, Iterable

import matplotlib.pyplot as plt
import numpy as np
from sympy import Eq, Inverse, Matrix, MatrixSymbol, diff, lambdify, symbols

from common import gradient, hessian, plot_result
from quasi_newton_method import quansi_newton_method


# %%
def augmented_lagrangian_method(
    f_sym,
    g_syms,
    initial_point,
    initial_alpha,
    initial_rho,
    criteria: float = 1e-4,
    max_iter: int = 4096,
):
    x = MatrixSymbol("x", 2, 1)
    u = MatrixSymbol("u", 1, len(g_syms))
    rho = symbols("rho")

    L_sym = (
        f_sym
        + sum([ui * gi.lhs for ui, gi in zip(u, g_syms)])
        + rho / 2 * sum([gi.lhs**2 for gi in g_syms])
    )

    g_conds = lambdify(x, g_syms[0].lhs)

    xs = [initial_point]
    cur_alpha = initial_alpha
    cur_rho = initial_rho
    cur_u = np.array([[0]])

    k = 0
    while criteria < abs(cur_rho * g_conds(xs[-1])):
        cur_L_sym = L_sym.replace(u, Matrix(cur_u)).replace(rho, cur_rho)
        xs.append(quansi_newton_method(cur_L_sym, xs[-1], cur_alpha)[-1])
        cur_u = cur_u + cur_rho * g_conds(xs[-1])
        cur_rho *= 2

        k += 1
        if max_iter < k:
            raise ValueError("not converged")

    return xs


# %%
if __name__ == "__main__":
    x = MatrixSymbol("x", 2, 1)
    f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
    f = lambdify([x], f_sym, "numpy")

    # %%
    g_syms = [Eq(x[0] ** 2 - x[1], 0, evaluate=False)]
    # %%
    initial_point = np.array([[2, 1]]).T
    initial_alpha = 1.0
    initial_rho = 1.0
    augmented_lagrangian_method_result = augmented_lagrangian_method(
        f_sym, g_syms, initial_point, initial_alpha, initial_rho
    )
    plot_result(f, augmented_lagrangian_method_result)
# %%
