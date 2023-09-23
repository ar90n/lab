# %%
from typing import Any, Callable, Iterable

from sympy.matrices.expressions import MatrixExpr
import numpy as np
from sympy import (
    Eq,
    Matrix,
    MatrixSymbol,
    lambdify,
    linear_eq_to_matrix,
    symbols,
)

from common import gradient, hessian, plot_result


# %%
def create_equations(f_sym, g_syms):
    x = list(f_sym.free_symbols)[0]
    dx = MatrixSymbol("dx", 2, 1)
    ds = MatrixSymbol("ds", 1, 1)
    du = MatrixSymbol("du", 1, 1)
    u = MatrixSymbol("u", 1, 1)
    s = MatrixSymbol("s", 1, 1)
    rho = symbols("rho")

    eqs = [
        Eq(
            hessian(f_sym, x) * dx
            + sum(
                [
                    ui * hessian(gi.lhs, x) * dx + dui * gradient(gi.lhs, x)
                    for ui, dui, gi in zip(u, du, g_syms)
                ],
                Matrix.zeros(2, 1),
            ),
            -gradient(f_sym, x)
            - sum(
                [ui * gradient(gi.lhs, x) for ui, gi in zip(u, g_syms)],
                Matrix.zeros(2, 1),
            ),
        ),
        *[
            Eq(ui * dsi + dui * si, rho - ui * si, evaluate=False)
            for ui, si, dui, dsi in zip(u, s, du, ds)
        ],
        *[
            Eq((gradient(gi.lhs, x).T * dx)[0] + dsi, -gi.lhs - si, evaluate=False)
            for si, dsi, gi in zip(s, ds, g_syms)
        ],
    ]

    scalar_eqs = []
    for eq in eqs:
        tmp = eq.lhs - eq.rhs
        if isinstance(tmp, MatrixExpr):
            scalar_eqs.append([*tmp])
        else:
            scalar_eqs.append([tmp])
    return sum(scalar_eqs, [])


# %%
def calc_alpha(s, ds, u, du):
    alpha_by_s = -s / ds
    alpha_by_u = -u / du
    bigger_alpha = max(alpha_by_s, alpha_by_u)
    smaller_alpha = min(alpha_by_s, alpha_by_u)

    if 0 < min(s, u):
        return bigger_alpha / 0.8
    elif max(s, u) < 0:
        return smaller_alpha * 0.8
    else:
        return bigger_alpha * 0.8


# %%
def interior_point_method(
    f_sym,
    g_syms,
    initial_point,
    initial_s,
    initial_u,
    initial_rho,
    initial_sigma,
    criteria: float = 1e-8,
    max_iter: int = 4096,
):
    x = list(f_sym.free_symbols)[0]
    u = MatrixSymbol("u", 1, 1)
    s = MatrixSymbol("s", 1, 1)
    dx = MatrixSymbol("dx", 2, 1)
    ds = MatrixSymbol("ds", 1, 1)
    du = MatrixSymbol("du", 1, 1)
    rho, sigma = symbols("rho sigma")

    eqs = create_equations(f_sym, g_syms)

    A_sym, b_sym = linear_eq_to_matrix(eqs, [*dx, *ds, *du])
    A_func = lambdify([x, s, u], A_sym)
    b_func = lambdify([x, s, u, rho], b_sym)

    rho_k_1_sym = sigma * u.T * s / s.shape[0]
    rho_k_1 = lambdify([sigma, u, s], rho_k_1_sym)

    k = 0
    xs = [initial_point]
    cur_s = initial_s
    cur_u = initial_u
    cur_rho = initial_rho
    cur_sigma = initial_sigma
    while criteria < cur_rho:
        cur_A = A_func(xs[-1], cur_s, cur_u)
        cur_b = b_func(xs[-1], cur_s, cur_u, cur_rho)
        ret = np.linalg.solve(cur_A, cur_b)

        cur_alpha = calc_alpha(cur_s, ret[2], cur_u, ret[3])

        xs.append(xs[-1] + cur_alpha * ret[:2])
        cur_s = cur_s + cur_alpha * ret[2]
        cur_u = cur_u + cur_alpha * ret[3]
        cur_rho = rho_k_1(cur_sigma, cur_u, cur_s)[0, 0]
        k += 1
        if max_iter < k:
            break
    return np.array(xs)


# %%
if __name__ == "__main__":
    x = MatrixSymbol("x", 2, 1)
    f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
    f = lambdify([x], f_sym)

    # %%
    g_syms = [Eq(x[0] ** 2 - x[1] + s[0], 0, evaluate=False)]
    # %%
    initial_point = np.array([[1, 2]]).T
    initial_s = np.array([[1]])
    initial_u = np.array([[0]])
    initial_rho = 1.0
    initial_sigma = 0.1
    interior_point_method_result = interior_point_method(
        f_sym, g_syms, initial_point, initial_s, initial_u, initial_rho, initial_sigma
    )
    plot_result(f, interior_point_method_result)
# %%
# eqs2
## %%
#    L_sym = (
#        f_sym
#        - rho * sum(log(si) for si in s)
#        + sum(ui * gi.lhs for ui, gi in zip(u, g_syms))
#    )
#
#    dL_dx_sym = gradient(L_sym, x)
#    dL_ds_sym = gradient(L_sym, s)
#    dL_su_sym = gradient(L_sym, u)
