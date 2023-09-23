# %%
import functools
from typing import Callable

import numpy as np
from sympy import And, Eq, Function, MatrixSymbol, diff, lambdify, solve, symbols

from common import gradient, plot_result


# %%
def secant_method_alpha_search(
    cond_func, update_alpha, alpha, max_iter: int = 4096
) -> float:
    prev_alpha, cur_alpha = 1.1 * alpha, alpha
    for _ in range(max_iter):
        cur_alpha, prev_alpha = (
            update_alpha(cur_alpha, prev_alpha),
            cur_alpha,
        )
        if cond_func(cur_alpha):
            break
    return cur_alpha


# %%
def armijo_condition(
    g: Callable[[float], float],
) -> bool:
    alpha, tau = symbols("alpha tau")
    grad_g_sym = diff(g, alpha)
    return g < g.replace(alpha, 0) + tau * alpha * grad_g_sym.replace(alpha, 0)


def wolfe_condition(
    g: Callable[[float], float],
) -> bool:
    alpha, tau, tau_1, tau_2 = symbols("alpha tau tau_1 tau_2")
    grad_g_sym = diff(g, alpha)

    armijo_expr = armijo_condition(g).replace(tau, tau_1)
    return And(armijo_expr, tau_2 * grad_g_sym.replace(alpha, 0) <= grad_g_sym)


# %%
def next_alpha(g):
    alpha, alpha_i, alpha_i_1, a, b, c = symbols("alpha alpha^i alpha^i-1 a b c")
    approx_g_sym = a * (alpha - b) ** 2 + c
    grad_approx_g_sym = diff(approx_g_sym, alpha)

    grad_g_func_sym = Function("g^{\prime}")
    _, update_alpha_sym = solve(
        [
            Eq(grad_g_func_sym(alpha_i), grad_approx_g_sym.replace(alpha, alpha_i)),
            Eq(grad_g_func_sym(alpha_i_1), grad_approx_g_sym.replace(alpha, alpha_i_1)),
        ],
        (a, b),
    )[0]

    grad_g_sym = diff(g, alpha)
    return update_alpha_sym.replace(
        grad_g_func_sym, lambda a: grad_g_sym.replace(alpha, a)
    )


def secant_method(
    f_sym,
    initial_point: np.array,
    initial_alpha: float,
    tau_1: float,
    tau_2: float,
    criteria: float = 1e-4,
    max_iter: int = 4096,
) -> np.array:
    x = list(f_sym.free_symbols)[0]
    alpha_sym, alpha_i_sym, alpha_i_1_sym, tau_1_sym, tau_2_sym = symbols(
        "alpha alpha^i alpha^i-1 tau_1 tau_2"
    )

    d_sym = -gradient(f_sym, x)
    g_sym = f_sym.replace(x, x + alpha_sym * d_sym).doit()

    d = lambdify([x], d_sym)
    alpha_update_func = lambdify(
        [x, alpha_i_sym, alpha_i_1_sym], next_alpha(g_sym), "numpy"
    )
    cond_func = lambdify(
        [x, tau_1_sym, tau_2_sym, alpha_sym], wolfe_condition(g_sym), "numpy"
    )

    k = 0
    xs = [initial_point]
    cur_alpha = initial_alpha
    while np.linalg.norm(d(xs[-1])) > criteria:
        cur_alpha_update_func = functools.partial(alpha_update_func, xs[-1])
        cur_cond_func = functools.partial(cond_func, xs[-1], tau_1, tau_2)
        cur_alpha = secant_method_alpha_search(
            cur_cond_func, cur_alpha_update_func, cur_alpha
        )
        xs.append(xs[-1] + cur_alpha * d(xs[-1]))

        k += 1
        if max_iter < k:
            raise ValueError(f"not converged: |d| = {np.linalg.norm(d(xs[-1]))}")

    return xs


# %%
def backtrack_line_search(
    f_sym,
    initial_point: np.array,
    initial_alpha: float,
    beta: float,
    criteria: float = 1e-4,
    max_iter: int = 4096,
) -> np.array:
    x = list(f_sym.free_symbols)[0]

    d_sym = -gradient(f_sym, x)
    d = lambdify([x], d_sym)

    k = 0
    xs = [initial_point]
    alpha = initial_alpha
    while np.linalg.norm(d(xs[-1])) > criteria:
        xs.append(xs[-1] + alpha * d(xs[-1]))
        alpha *= beta

        k += 1
        if max_iter < k:
            raise ValueError(f"not converged: |d| = {np.linalg.norm(d(xs[-1]))}")

    return xs


# %%
x = MatrixSymbol("x", 2, 1)
f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
f = lambdify([x], f_sym)

# %%
if __name__ == "__main__":
    secant_method_results = secant_method(
        f_sym,
        initial_point=np.array([[0, 3]]).T,
        initial_alpha=1.0,
        tau_1=0.3,
        tau_2=0.5,
    )

    # %%
    plot_result(f, secant_method_results)
    # %%
    backtrack_line_search_results = backtrack_line_search(
        f_sym,
        initial_point=np.array([[0, 3]]).T,
        initial_alpha=0.0625,
        beta=0.9999,
    )
    # %%
    plot_result(f, backtrack_line_search_results)
# %%
