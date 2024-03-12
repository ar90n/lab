from sympy import Basic, Symbol


def get_non_positive_variables(expr: Basic) -> list[Basic]:
    return [s for s in expr.free_symbols if s.is_nonpositive]


def get_negative_variables(expr: Basic) -> list[Basic]:
    return [s for s in expr.free_symbols if s.is_negative]


def is_non_restricted_variable(s: Basic) -> bool:
    return (
        s.is_nonnegative is None
        and s.is_positive is None
        and s.is_negative is None
        and s.is_nonpositive is None
    )


def get_non_restricted_variables(expr: Basic) -> list[Basic]:
    return [s for s in expr.free_symbols if is_non_restricted_variable(s)]

def is_split_variable(s: Symbol) -> bool:
    return s.name.endswith("_p") or s.name.endswith("_m")