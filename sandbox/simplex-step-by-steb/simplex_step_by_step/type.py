from dataclasses import dataclass

from sympy.core.expr import Expr
from sympy import Rel, Ge, Eq, Symbol


@dataclass(slots=True, frozen=True)
class Maximize:
    expr: Expr


@dataclass(slots=True, frozen=True)
class Minimize:
    expr: Expr


ObjectiveFunction = Maximize | Minimize
SubjectTo = Rel


@dataclass(slots=True, frozen=True)
class LPP:
    obj: ObjectiveFunction
    subject_to: tuple[SubjectTo]


@dataclass(slots=True, frozen=True)
class NormalizedLPP:
    obj: Maximize
    subject_to: tuple[Ge]


@dataclass(slots=True, frozen=True)
class Dictionary:
    z: Expr
    subject_to: tuple[Eq]

@dataclass(slots=True, frozen=True)
class Solution:
    values: dict[Symbol, float]