# %%
import functools
from typing import Callable
from sympy import symbols, diff, lambdify, Eq, solve, Function, Matrix, MatrixSymbol
from sympy.vector import gradient
import numpy as np
import functools
import matplotlib.pyplot as plt


# %%
def gradient(f, x):
    return Matrix([f]).jacobian(x).T


# %%
def is_armijo_condition_satisfied(
    g: Callable[[float], float],
    grad_g: Callable[[float], float],
    tau: float,
    alpha: float,
) -> bool:
    return g(alpha) < g(0) + tau * alpha * grad_g(0)


def is_wolfe_condition_satisfied(
    g: Callable[[float], float],
    grad_g: Callable[[float], float],
    tau_1: float,
    tau_2: float,
    alpha: float,
) -> bool:
    assert tau_1 < tau_2
    return is_armijo_condition_satisfied(g, grad_g, tau_1, alpha) and (
        tau_2 * grad_g(0) <= grad_g(alpha)
    )


# %%


def secant_method_alpha_search(
    cond_func, update_alpha, alpha, max_iter: int = 4096
) -> float:
    prev_alpha, cur_alpha = 1.10, alpha
    for _ in range(max_iter):
        cur_alpha, prev_alpha = (
            update_alpha(cur_alpha, prev_alpha),
            cur_alpha,
        )
        if cond_func(cur_alpha):
            break
    return cur_alpha


def secant_method(
    d,
    g,
    grad_g,
    initial_point: np.array,
    initial_alpha: float,
    tau_1: float,
    tau_2: float,
    criteria: float = 1e-4,
    max_iter: int = 4096,
) -> np.array:
    k = 0
    xs = [initial_point]
    alpha = initial_alpha
    while np.linalg.norm(d(xs[-1])) > criteria:
        if max_iter < k:
            raise ValueError(f"not converged: |d| = {np.linalg.norm(d(xs[-1]))}")

        cur_g = functools.partial(g, xs[-1])
        cur_grad_g = functools.partial(grad_g, xs[-1])
        cur_update_alpha = functools.partial(secant_method_alpha_update, cur_grad_g)
        cur_cond_func = functools.partial(
            is_wolfe_condition_satisfied, cur_g, cur_grad_g, tau_1, tau_2
        )
        # cur_cond_func = functools.partial(is_armijo_condition_satisfied, cur_g, cur_grad_g, tau_1)

        alpha = secant_method_alpha_search(cur_cond_func, cur_update_alpha, alpha)
        xs.append(xs[-1] + alpha * d(xs[-1]))
        k += 1
    return xs


# %%
def backtrack_line_search(
    d,
    initial_point: np.array,
    initial_alpha: float,
    beta: float,
    criteria: float = 1e-4,
    max_iter: int = 4096,
) -> np.array:
    k = 0
    xs = [initial_point]
    alpha = initial_alpha
    while np.linalg.norm(d(xs[-1])) > criteria:
        if max_iter < k:
            raise ValueError(f"not converged: |d| = {np.linalg.norm(d(xs[-1]))}")

        xs.append(xs[-1] + alpha * d(xs[-1]))
        alpha *= beta
        k += 1
    return xs


# %%
alpha = symbols("alpha")
x = MatrixSymbol("x", 2, 1)

# %%
f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
d_sym = -gradient(f_sym, x)
g_sym = f_sym.replace(x, x + alpha * d_sym).doit()

# %%
f = lambdify([x], f_sym)
d = lambdify([x], d_sym)
g = lambdify([x, alpha], g_sym)
grad_f = lambdify([x], gradient(f_sym, x))
grad_g = lambdify([x, alpha], diff(g_sym, alpha))

# %%
alpha_i, alpha_i_1, a, b, c = symbols("alpha^i alpha^i-1 a b c")
approx_g_sym = a * (alpha - b) ** 2 + c
grad_approx_g_sym = diff(approx_g_sym, alpha)

grad_g_func_sym = Function("g'")
eq_1 = Eq(grad_g_func_sym(alpha_i), grad_approx_g_sym.replace(alpha, alpha_i))
eq_2 = Eq(grad_g_func_sym(alpha_i_1), grad_approx_g_sym.replace(alpha, alpha_i_1))
_, solved_b_sym = solve((eq_1, eq_2), (a, b))[0]
# %%
_grad_g_func_sym = Function("grad_g")
secant_method_alpha_update = lambdify(
    [_grad_g_func_sym, alpha_i, alpha_i_1],
    solved_b_sym.subs(grad_g_func_sym, _grad_g_func_sym),
)


# %%
if __name__ == "__main__":
    # %%
    xs = secant_method(
        d,
        g,
        grad_g,
        initial_point=np.array([[0, 3]]).T,
        initial_alpha=1.0,
        tau_1=0.3,
        tau_2=0.5,
    )
    # %%
    xs = backtrack_line_search(
        d, initial_point=np.array([[0, 3]]).T, initial_alpha=0.0625, beta=0.9999
    )

    # %%
    N = 128
    X, Y = np.meshgrid(np.linspace(-1, 5, N), np.linspace(-1, 5, N))
    ps = np.stack([X, Y]).transpose([1, 2, 0])
    Z = np.apply_along_axis(
        lambda p: f(p.reshape(2, 1)), -1, ps.reshape(-1, 2)
    ).reshape(N, N)
    fig, ax = plt.subplots()
    C = 10
    levels = np.linspace(0, (np.max(Z) - np.min(Z)) ** 0.5, C) ** 2 + np.min(Z)
    ax.contour(X, Y, Z, levels, colors="b")
    ax.set_aspect("equal", "box")

    # x=0, y=0の位置にx軸とy軸を表示
    ax.spines["left"].set_position("zero")  # y軸の位置を0に設定
    ax.spines["right"].set_color("none")  # 右の枠線を非表示
    ax.spines["bottom"].set_position("zero")  # x軸の位置を0に設定
    ax.spines["top"].set_color("none")  # 上の枠線を非表示

    # x軸、y軸のラベルを設定
    ax.xaxis.set_label_coords(0, 0)
    ax.yaxis.set_label_coords(0, 0)
    ax.set_xlabel("x1", fontsize=14)
    ax.set_ylabel("x2", fontsize=14)

    L = 4
    for i, (pt, last_pt) in enumerate(zip(xs[:L], [0, *xs[: L - 1]])):
        ax.scatter(*pt, color="red", s=20, edgecolors="black", zorder=100)

        if i == 0:
            ax.text(
                pt[0] - 0.1,
                pt[1] + 0.15,
                f"$x^{i}$",
                fontsize=8,
                bbox=dict(facecolor="white"),
            )
        if 0 < i:
            ax.annotate(
                "",
                xy=pt,
                xycoords="data",
                xytext=last_pt,
                textcoords="data",
                arrowprops=dict(arrowstyle="-", connectionstyle="arc3", color="red"),
                zorder=50,
            )

    ax.scatter(*xs[-1], color="green", s=20, edgecolors="black", zorder=100)
    ax.annotate(
        "",
        xy=xs[-1],
        xycoords="data",
        xytext=xs[L - 1],
        textcoords="data",
        arrowprops=dict(
            arrowstyle="-", linestyle="dotted", connectionstyle="arc3", color="red"
        ),
        zorder=50,
    )
    ax.text(
        xs[-1][0] - 0.1,
        xs[-1][1] + 0.15,
        f"$x^{{{len(xs) - 1}}}$",
        fontsize=8,
        bbox=dict(facecolor="white"),
    )

    plt.show()
# %%
