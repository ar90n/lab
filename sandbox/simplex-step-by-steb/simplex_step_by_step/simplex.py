# %%
import sys
sys.path.append("../")
from typing import Callable
import sympy as sp
from sympy import Symbol, Eq, solve
from IPython.display import display, Math, Latex

from simplex_step_by_step.type import LPP, Dictionary, Maximize, Minimize, Solution
from simplex_step_by_step.preprocess import normalize
from simplex_step_by_step.utils import is_split_variable


def has_basic_feasible_solution(d: Dictionary) -> bool:
    sol = calc_solution(d)
    return all(s >= 0 for s in sol.values.values())


def is_optimal_solution(d: Dictionary) -> bool:
    return all(c <= 0 for c in d.z.as_coeff_Add()[1].as_coefficients_dict().values())


def make_dictionary(lpp: LPP) -> Dictionary:
    new_subject_to = [
        Eq(Symbol(f"s_{i}", nonnegative=True), subj_to.rhs - subj_to.lhs)
        for i, subj_to in enumerate(lpp.subject_to)
    ]
    return Dictionary(lpp.obj.expr, tuple(new_subject_to))


def calc_solution(d: Dictionary) -> Solution:
    nonbasic_variables = set().union(
        *[s.rhs.free_symbols for s in d.subject_to if isinstance(s, Eq)]
    )
    ret = {v: 0 for v in nonbasic_variables}
    for eq in d.subject_to:
        if isinstance(eq, Eq):
            ret[eq.lhs] = eq.rhs.subs({v: 0 for v in nonbasic_variables})
    return Solution(ret)


def select_entering_variable(d: Dictionary) -> Symbol:
    return max(d.z.free_symbols, key=lambda s: d.z.coeff(s))


def select_leaving_variable(d: Dictionary, entering: Symbol) -> Symbol:
    candidates = []
    for eq in d.subject_to:
        c, vs = eq.rhs.as_coeff_Add()
        if entering in vs.free_symbols:
            m = -c / vs.coeff(entering)
            if m < 0:
                m = float("inf")
            candidates.append((m, eq.lhs))
    return min(candidates, key=lambda x: x[0])[1]


# %%
def pivot(d: Dictionary, entering: Symbol, leaving: Symbol, verbose: bool = True) -> Dictionary:
    if verbose:
        display_simplex((entering, leaving))
    pivot_expr = next(s for s in d.subject_to if s.lhs == leaving)
    pivot_expr = Eq(entering, solve(pivot_expr, entering)[0])
    new_z = d.z.subs(entering, pivot_expr.rhs)
    new_subject_to = []
    for eq in d.subject_to:
        if eq.lhs != leaving:
            new_subject_to.append(eq.subs(entering, pivot_expr.rhs))
        else:
            new_subject_to.append(pivot_expr)
    return Dictionary(new_z, new_subject_to)


def calc_objective_value(lpp: LPP, sol: Solution) -> float:
    return lpp.obj.expr.subs(sol.values)


# %%
def has_unbounded_solution(d: Dictionary) -> bool:
    fvs = set().union(*[s.rhs.free_symbols for s in d.subject_to])
    for s in fvs:
        zc = d.z.coeff(s)
        scs = [eq.rhs.coeff(s) for eq in d.subject_to]
        if all([0 <= c for c in [zc, *scs]]):
            return True
    return False


# %%
def make_auxiliary_problem(d: Dictionary) -> Dictionary:
    leaving = min(calc_solution(d).values.items(), key=lambda x: x[1])[0]

    new_z = Symbol("a", nonnegative=True)
    new_subject_to = list(d.subject_to)
    for i, eq in enumerate(d.subject_to):
        new_subject_to[i] = Eq(eq.lhs, eq.rhs + new_z)
    ret = Dictionary(-new_z, tuple(new_subject_to))
    ret = pivot(ret, new_z, leaving, False)
    return ret, new_z


def merge_split_variables(d: Dictionary) -> Dictionary:
    def _trim_suffix(s: str) -> str:
        return s.removesuffix("_p").removesuffix("_m")

    merge_variable_names = {
        _trim_suffix(f.name) for f in d.z.free_symbols if is_split_variable(f)
    }
    for eq in d.subject_to:
        merge_variable_names = merge_variable_names.union(
            {_trim_suffix(f.name) for f in eq.rhs.free_symbols if is_split_variable(f)}
        )

    new_z = d.z
    new_subject_to = list(d.subject_to)
    for vn in merge_variable_names:
        v = Symbol(vn)
        vp = Symbol(vn + "_p", nonnegative=True)
        vm = Symbol(vn + "_m", nonnegative=True)
        new_z = new_z.subs(vp, v + vm)
        for i, eq in enumerate(d.subject_to):
            new_subject_to[i] = new_subject_to[i].subs(vp, v + vm)
    return Dictionary(new_z, tuple(new_subject_to))


def trim_slack_variables(sol: Solution) -> Solution:
    return Solution({k: v for k, v in sol.values.items() if not k.name.startswith("s")})

# %%
def display_lpp(lpp: LPP):
    obj = r"\text{maximize}" if isinstance(lpp.obj, Maximize) else r"\text{minimize}"
    subject_to = r"\\ & &".join([sp.latex(s) for s in lpp.subject_to])
    display(Latex(rf"""\begin{{equation*}}
      \begin{{aligned}}
          & {obj}
              & {sp.latex(lpp.obj.expr)} \\
          & \text{{subject to}}
              & {subject_to} \\
      \end{{aligned}}
    \end{{equation*}}"""))

def display_dictionary(d: Dictionary):
    subject_to = r"\\ &".join([sp.latex(s) for s in d.subject_to])
    display(Latex(rf"""\begin{{equation*}}
      \begin{{aligned}}
              & {sp.latex(Eq(Symbol('z'), d.z))} \\
              & {subject_to} \\
      \end{{aligned}}
    \end{{equation*}}"""))

def display_text(s: str):
    content = rf"\text{{{s}}}"
    display(Latex(rf"""\begin{{equation*}} {content} \end{{equation*}}"""))

def display_solution(sol: Solution):
    variables = r",".join([sp.latex(k) for k in sol.values.keys()])
    values = r",".join([sp.latex(v) for v in sol.values.values()])
    display(Latex(rf"""\begin{{equation*}}
      \begin{{aligned}}
            {variables} = {values} \\
      \end{{aligned}}
    \end{{equation*}}"""))

def display_pivot(enter_leaving: tuple[Symbol, Symbol]):
    display(Latex(rf"""\begin{{equation*}}
      \begin{{aligned}}
            & \text{{entering variable}} = {sp.latex(enter_leaving[0])} \\
            & \text{{leaving variable}} = {sp.latex(enter_leaving[1])} \\
      \end{{aligned}}
    \end{{equation*}}"""))

def display_simplex(v: any):
    if isinstance(v, Dictionary):
        display_dictionary(v)
    elif isinstance(v, LPP):
        display_lpp(v)
    elif isinstance(v, Solution):
        display_solution(v)
    elif isinstance(v, tuple):
        display_pivot(v)
    else:
        display_text(v)

display_simplex(lpp)
nl = normalize(lpp)
display_simplex(nl)
d = make_dictionary(nl)
display_simplex(d)   
display_simplex("calc solution")
sol = calc_solution(d)
display_solution(sol)
# %%
def simplex(lpp: LPP) -> dict[Symbol, float]:
    def _fist_stage_simplex(d: Dictionary) -> Dictionary:
        aux_d, av = make_auxiliary_problem(d)
        display_simplex("make auxiliary problem")
        display_simplex(aux_d)

        aux_d = _solve(aux_d)
        display_simplex("solve auxiliary problem")
        display_simplex(aux_d)

        sol = calc_solution(aux_d)
        display_simplex("get following solution")
        display_simplex(sol)

        if sol.values[av] != 0:
            raise ValueError("No basic feasible solution")

        new_subject_to = tuple([eq.subs(av, 0) for eq in aux_d.subject_to])
        new_z = d.z
        for eq in new_subject_to:
            if isinstance(eq, Eq):
                new_z = new_z.subs(eq.lhs, eq.rhs)
        ret = Dictionary(new_z, new_subject_to)
        display_simplex("assign 0 to a and then get following dictionary")
        display_simplex(ret)

        return ret

    def _solve(d: Dictionary) -> Dictionary:
        #display_simplex(d.z, d.subject_to, calc_solution(d))
        if not has_basic_feasible_solution(d):
            display_simplex("have non basic feasible solution. start first stage simplex method.")
            d = _fist_stage_simplex(d)
        if not has_basic_feasible_solution(d):
            raise ValueError("No basic feasible solution")
        if has_unbounded_solution(d):
            raise ValueError("has unbounded solution")

        #display_simplex(d.z, d.subject_to, calc_solution(d))
        while not is_optimal_solution(d):
            if not has_basic_feasible_solution(d):
                raise ValueError("No basic feasible solution")
            if has_unbounded_solution(d):
                raise ValueError("has unbounded solution")

            ev = select_entering_variable(d)
            lv = select_leaving_variable(d, ev)
            d = pivot(d, ev, lv)
            display_simplex(d)
        return d

    display_simplex(lpp)

    nl = normalize(lpp)
    display_simplex("make standard form")
    display_simplex(nl)

    d = make_dictionary(nl)
    display_simplex("make dictionary")
    display_simplex(d)

    ret = calc_solution(d)
    display_simplex("get following solution")
    display_simplex(ret)

    d = _solve(d)

    split_variables = {
        f for f in d.z.free_symbols if is_split_variable(f)
    }
    for eq in d.subject_to:
        split_variables = split_variables.union(
            {f for f in eq.rhs.free_symbols if is_split_variable(f)}
        )
    if 0 < len(split_variables):
        d = merge_split_variables(d)
        display_simplex("merge split variables")
        display_simplex(d)

    ret = calc_solution(d)
    ret = trim_slack_variables(ret)
    return ret

# %%
# 2-4 1
x_1 = Symbol("x_1", nonnegative=True)
x_2 = Symbol("x_2", nonnegative=True)
x_3 = Symbol("x_3", nonnegative=True)
lpp = LPP(
    Maximize(4 * x_1 + 8 * x_2 + 10 * x_3),
    (
        x_1 + x_2 + x_3 <= 20,
        3 * x_1 + 4 * x_2 + 6 * x_3 <= 100,
        4 * x_1 + 5 * x_2 + 3 * x_3 <= 100,
    ),
)
# %%
# 2-4 2
x_1 = Symbol("x_1", nonnegative=True)
x_2 = Symbol("x_2", nonnegative=True)
x_3 = Symbol("x_3", nonnegative=True)
lpp = LPP(
    Maximize(x_1 + 3 * x_2 - x_3),
    (
        2 * x_1 + 2 * x_2 - x_3 <= 10,
        3 * x_1 - 2 * x_2 + x_3 <= 10,
        x_1 - 3 * x_2 + x_3 <= 10,
    ),
)
# %%
# 2-4 3
x_1 = Symbol("x_1", nonnegative=True)
x_2 = Symbol("x_2", nonnegative=True)
lpp = LPP(Maximize(10 * x_1 + x_2), (x_1 <= 1, 20 * x_1 + x_2 <= 100))
# %%
# 2-5 1
x_1 = Symbol("x_1", nonnegative=True)
x_2 = Symbol("x_2", nonnegative=True)
x_3 = Symbol("x_3")
lpp = LPP(
    Minimize(2 * x_1 + 3 * x_2 + x_3),
    (x_1 + 4 * x_2 + 2 * x_3 >= 8, 3 * x_1 + 2 * x_2 >= 6),
)
# %%
# 2-5 2
x_1 = Symbol("x_1", nonnegative=True)
x_2 = Symbol("x_2", nonnegative=True)
x_3 = Symbol("x_3", nonnegative=True)
lpp = LPP(
    Maximize(x_1 - x_2 + x_3),
    (
        2 * x_1 - x_2 + 2 * x_3 <= 4,
        2 * x_1 - 3 * x_2 + x_3 <= -5,
        -x_1 + x_2 - 2 * x_3 <= -1,
    ),
)


# %%
# 2-5 3
x_1 = Symbol('x_1', nonnegative=True)
x_2 = Symbol('x_2', nonnegative=True)
lpp = LPP(Maximize(-x_1 + x_2), (-x_1 + x_2 >= 1, x_1 + x_2 >= 3, 2 * x_1 + x_2 <= 2 ))
# %%
sol = simplex(lpp)
obj_value = calc_objective_value(lpp, sol)
display_simplex("find solution")
display_simplex(sol)
display_simplex(f"objective value: {obj_value}")
# %%
