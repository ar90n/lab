# %%
from typing import Any, Callable, Iterable

import numpy as np
from sympy import MatrixSymbol, lambdify

from common import gradient, hessian, plot_result


# %%
def newton_method(
    f_sym,
    initial_point: np.array,
    criteria: float = 1e-4,
    max_iter: int = 4096,
) -> np.ndarray:
    x = list(f_sym.free_symbols)[0]

    grad_f_sym = gradient(f_sym, x)
    d_sym = -hessian(f_sym, x).inv() * gradient(f_sym, x)

    grad_f = lambdify([x], grad_f_sym, "numpy")
    d = lambdify([x], d_sym, "numpy")

    k = 0
    xs = [initial_point]
    while criteria < np.linalg.norm(grad_f(xs[-1])):
        xs.append(xs[-1] + d(xs[-1]))

        k += 1
        if max_iter < k:
            raise ValueError(f"not converged: |d| = {np.linalg.norm(d(xs[-1]))}")

    return np.array(xs)


# %%
if __name__ == "__main__":
    x = MatrixSymbol("x", 2, 1)
    f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
    f = lambdify([x], f_sym, "numpy")

    # %%
    initial_point = np.array([[0, 3]]).T
    newton_method_result = newton_method(f_sym, initial_point)
    plot_result(f, newton_method_result)
