from sympy import Mul, Basic, Symbol, simplify, Ge, Gt
from simplex_step_by_step.type import LPP, Maximize, Minimize
from simplex_step_by_step.utils import (
    get_non_positive_variables,
    get_negative_variables,
    get_non_restricted_variables,
)


def _convert_to_maximization_problem(lpp: LPP) -> LPP:
    if isinstance(lpp.obj, Maximize):
        return lpp

    return LPP(Maximize(-lpp.obj.expr), lpp.subject_to)


def _make_all_variables_positive(lpp: LPP) -> LPP:
    new_expr = lpp.obj.expr
    new_subject_to = list(lpp.subject_to)

    all_non_positive_variables = [
        get_non_positive_variables(e) for e in [lpp.obj.expr, *lpp.subject_to]
    ]
    all_negative_variables = [
        get_negative_variables(e) for e in [lpp.obj.expr, *lpp.subject_to]
    ]
    all_candidates_variables: list[Basic] = set(
        sum([*all_non_positive_variables, *all_negative_variables], [])
    )
    for v in all_candidates_variables:
        vp = -Symbol(str(v), nonnegative=True)
        new_expr = new_expr.subs(v, vp)
        for i, ineq in enumerate(new_subject_to):
            new_subject_to[i] = ineq.subs(v, vp)

    new_obj = (
        Maximize(new_expr) if isinstance(lpp.obj, Maximize) else Minimize(new_expr)
    )
    return LPP(new_obj, tuple(new_subject_to))


def _split_non_restricted_variables(lpp: LPP) -> LPP:
    new_expr = lpp.obj.expr
    new_subject_to = list(lpp.subject_to)

    all_non_restricted_variables = [
        get_non_restricted_variables(e) for e in [lpp.obj.expr, *lpp.subject_to]
    ]
    all_non_restricted_variables = set(sum(all_non_restricted_variables, []))
    for v in all_non_restricted_variables:
        vp = Symbol(str(v) + "_p", nonnegative=True)
        vm = Symbol(str(v) + "_m", nonnegative=True)
        new_expr = new_expr.subs(v, vp - vm)
        for i, ineq in enumerate(new_subject_to):
            new_subject_to[i] = ineq.subs(v, vp - vm)
    new_obj = (
        Maximize(new_expr) if isinstance(lpp.obj, Maximize) else Minimize(new_expr)
    )
    return LPP(new_obj, tuple(new_subject_to))


def _rearrange_subject_to(lpp: LPP) -> LPP:
    new_subject_to = list(lpp.subject_to)
    for i, subj_to in enumerate(new_subject_to):
        lhs_c, lhs_x = subj_to.lhs.as_coeff_Add()
        rhs_c, rhs_x = subj_to.rhs.as_coeff_Add()
        lhs = simplify(lhs_x - rhs_x)
        rhs = simplify(rhs_c - lhs_c)
        new_subject_to[i] = new_subject_to[i].func(lhs, rhs)
        if isinstance(new_subject_to[i], Ge) or isinstance(new_subject_to[i], Gt):
            new_subject_to[i] = new_subject_to[i].reversedsign
    return LPP(lpp.obj, tuple(new_subject_to))


def normalize(lpp: LPP) -> LPP:
    lpp = _convert_to_maximization_problem(lpp)
    lpp = _rearrange_subject_to(lpp)
    lpp = _make_all_variables_positive(lpp)
    lpp = _split_non_restricted_variables(lpp)
    return lpp
