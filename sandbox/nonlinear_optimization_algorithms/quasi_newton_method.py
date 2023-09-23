# %%
import functools
from typing import Any, Callable, Iterable

import numpy as np
from sympy import Inverse, MatrixSymbol, lambdify, symbols

from common import gradient, plot_result
from gradient_descent import next_alpha, secant_method_alpha_search, wolfe_condition


# %%
def next_B(grad_f_sym):
    B = MatrixSymbol("B", 2, 2)
    s = MatrixSymbol("s", 2, 1)
    y = MatrixSymbol("y", 2, 1)
    x = MatrixSymbol("x", 2, 1)
    x_i = MatrixSymbol("x_i", 2, 1)
    x_i_1 = MatrixSymbol("x_i_1", 2, 1)

    bfgs_sym = (
        B - ((B * s) * (B * s).T) / (s.T * B * s)[0, 0] + (y * y.T) / (s.T * y)[0, 0]
    )
    bfgs_sym = bfgs_sym.replace(s, x_i - x_i_1)
    bfgs_sym = bfgs_sym.replace(
        y, grad_f_sym.replace(x, x_i) - grad_f_sym.replace(x, x_i_1)
    )
    return bfgs_sym


# %%
def quansi_newton_method(
    f_sym,
    initial_point: np.array,
    initial_alpha: float,
    tau_1: float = 0.3,
    tau_2: float = 0.5,
    criteria: float = 1e-4,
    max_iter: int = 4096,
) -> np.array:
    #x = list(f_sym.free_symbols)[0]
    x = MatrixSymbol("x", 2, 1)
    B = MatrixSymbol("B", 2, 2)
    alpha_sym, alpha_i_sym, alpha_i_1_sym, tau_1_sym, tau_2_sym, x_i, x_i_1 = symbols(
        "alpha alpha^i alpha^i-1 tau_1 tau_2 x_i x_i_1"
    )

    grad_f_sym = gradient(f_sym, x)
    d_sym = -Inverse(B) * grad_f_sym
    g_sym = f_sym.replace(x, x + alpha_sym * d_sym).doit()

    grad_f = lambdify([x], grad_f_sym, "numpy")

    alpha_update_func = lambdify(
        [x, B, alpha_i_sym, alpha_i_1_sym], next_alpha(g_sym), "numpy"
    )
    B_update_func = lambdify([B, x_i, x_i_1], next_B(grad_f_sym), "numpy")
    cond_func = lambdify(
        [x, tau_1_sym, tau_2_sym, B, alpha_sym], wolfe_condition(g_sym), "numpy"
    )

    k = 0
    xs = [initial_point]
    cur_B = np.eye(initial_point.shape[0])
    cur_alpha = initial_alpha

    while criteria < np.linalg.norm(grad_f(xs[-1])):
        cur_alpha_update_func = functools.partial(alpha_update_func, xs[-1], cur_B)
        cur_cond_func = functools.partial(cond_func, xs[-1], tau_1, tau_2, cur_B)
        cur_alpha = secant_method_alpha_search(
            cur_cond_func, cur_alpha_update_func, cur_alpha
        )

        next_x = xs[-1] - cur_alpha * np.linalg.inv(cur_B) @ grad_f(xs[-1])
        cur_B = B_update_func(cur_B, next_x, xs[-1])
        xs.append(next_x)

        k += 1
        if k == max_iter:
            raise ValueError("not converged")

    return np.array(xs)


# %%
if __name__ == "__main__":
    x = MatrixSymbol("x", 2, 1)
    f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
    f = lambdify([x], f_sym, "numpy")

    # %%
    initial_point = np.array([[0, 3]]).T
    initial_alpha = 1.0
    newton_method_result = quansi_newton_method(f_sym, initial_point, initial_alpha)
    plot_result(f, newton_method_result)
