# %%
from typing import Any, Callable, Iterable

import numpy as np
from sympy import Lt, MatrixSymbol, lambdify, linear_eq_to_matrix

from common import gradient, plot_result


# %%
def active_set_method(
    f_sym,
    g_syms,
    initial_point: np.array,
    max_iter: int = 4096,
):
    M = len(g_syms)
    x = list(f_sym.free_symbols)[0]
    u = MatrixSymbol("u", 1, M)

    L_sym = f_sym + sum([ui * gi.lhs for ui, gi in zip(u, g_syms)])

    A, b = linear_eq_to_matrix([*gradient(L_sym, u), *gradient(L_sym, x)], [*u, *x])
    A = np.array(A).astype(np.float64)
    b = np.array(b).astype(np.float64)

    xs = [initial_point]
    actives = np.where(np.abs(A[:-2, -2:] @ xs[-1] - b[:-2]) < 1e-12)[0]
    for _ in range(max_iter):
        active_indice = [*actives, -2, -1]

        ans = np.linalg.solve(A[active_indice, :][:, active_indice], b[active_indice])
        cur_u = ans[:-2]
        cur_x = ans[-2:]

        if np.linalg.norm(cur_x - xs[-1]) < 1e-12:
            if np.all(cur_u >= 0):
                break

            min_index = np.argmin(cur_u)
            actives = np.array([a for i, a in enumerate(actives) if i != min_index])
        elif not np.all(A[:-2, -2:] @ cur_x - b[:-2] < 1e-12):
            orig = xs[-1]
            d = cur_x - orig
            alpha = -float("inf")
            for i in range(M):
                a_i = A[i : i + 1, -2:].T
                A_i = np.hstack([d, np.array([[0, 1], [-1, 0]]) @ a_i])
                if abs(np.linalg.det(A_i)) < 1e-12:
                    continue

                max_a_index = np.argmax(np.abs(a_i))
                orig_i = np.zeros(2)
                orig_i[max_a_index] = b[i, 0] / a_i[max_a_index, 0]

                O_i = orig_i - orig.flatten()
                alpha_i = np.linalg.solve(A_i, O_i)[0]

                x_i = orig + alpha_i * d
                if np.all(A[:-2, -2:] @ x_i - b[:-2] < 1e-12):
                    alpha = max(alpha, alpha_i)
            cur_x = orig + alpha * d
            actives = np.where(np.abs(A[:-2, -2:] @ cur_x - b[:-2]) < 1e-12)[0]
        xs.append(cur_x)
    return xs


# %%
if __name__ == "__main__":
    x = MatrixSymbol("x", 2, 1)
    f_sym = (x[0] - 1) ** 2 + (x[1] - 2.5) ** 2
    f = lambdify([x], f_sym, "numpy")
    # %%
    g_syms = [
        Lt(-x[0] + 2 * x[1] - 2, 0, evaluate=False),
        Lt(x[0] + 2 * x[1] - 6, 0, evaluate=False),
        Lt(x[0] - 2 * x[1] - 2, 0, evaluate=False),
        Lt(-x[0], 0, evaluate=False),
        Lt(-x[1], 0, evaluate=False),
    ]

    # %%
    initial_point = np.array([[2, 0]]).T
    active_set_method_result = active_set_method(f_sym, g_syms, initial_point)
    plot_result(f, active_set_method_result)
# %%
