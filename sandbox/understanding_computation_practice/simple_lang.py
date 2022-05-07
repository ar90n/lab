import tatsu
from tatsu.ast import AST
from typing import Protocol, TypeAlias
from functools import reduce

import pytest

Value: TypeAlias = int | bool
Environment: TypeAlias = dict[str, "Expression"]

GRAMMER = r"""
    start = sequence $ ;

    sequence = {statement}+ ;
    while_ = "while" '(' condition:expression ')' '{' body:sequence '}' ;
    if_ = "if" '(' condition:expression ')' '{' consequence:sequence '}' ['else' '{' alternative:sequence '}'] ;
    statement = | assign | while_ | if_ ;
    assign = name:variable "=" value:expression ;
    expression = | add | less_than | term ;
    term = | multiply | factor ;
    less_than = left:term '<' right:expression ;
    add = left:term '+' right:expression ;
    multiply = left:factor '*' right:term ;
    factor = |'(' expression ')' | number | bool | variable ;
    number = /\d+/ ;
    variable = /[a-zA-Z_][a-zA-Z0-9_]*/ ;
    bool = |'true' | 'false' ;
"""


class Expression(Protocol):
    def reduce(self, env: Environment) -> "Expression":
        ...

    def evaluate(self, env: Environment) -> "Expression":
        ...

    def value(self) -> Value | None:
        return None

    def __str__(self) -> str:
        ...

    def __repr__(self) -> str:
        ...


class Statement(Protocol):
    def reduce(self, env: Environment) -> tuple["Statement", Environment]:
        ...

    def evaluate(self, env: Environment) -> Environment:
        ...

    def __str__(self) -> str:
        ...

    def __repr__(self) -> str:
        ...


class Number(Expression):
    __match_args__ = ("_value",)

    def __init__(self, value: int):
        self._value = value

    def reduce(self, env: Environment) -> Expression:
        return self

    def evaluate(self, env: Environment) -> Expression:
        return self

    def value(self) -> Value | None:
        return self._value

    def __str__(self) -> str:
        return f"{self.value()}"

    def __repr__(self) -> str:
        return f"Number({self.value()})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Number):
            return self.value() == other.value()
        return False


class Boolean(Expression):
    __match_args__ = ("_value",)

    def __init__(self, value: bool):
        self._value = value

    def reduce(self, env: Environment) -> Expression:
        return self

    def evaluate(self, env: Environment) -> Expression:
        return self

    def value(self) -> Value | None:
        return self._value

    def __str__(self) -> str:
        return f"{self.value()}"

    def __repr__(self) -> str:
        return f"Boolean({self.value()})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Boolean):
            return self.value() == other.value()
        return False


class Add(Expression):
    def __init__(self, left: Expression, right: Expression):
        self.left = left
        self.right = right

    def reduce(self, env: Environment) -> Expression:
        match (self.left.value(), self.right.value()):
            case (None, _):
                return Add(self.left.reduce(env), self.right)
            case (_, None):
                return Add(self.left, self.right.reduce(env))
            case (left, right):
                return Number(left + right)  # type: ignore

    def evaluate(self, env: Environment) -> Expression:
        left_value = self.left.evaluate(env).value()
        right_value = self.right.evaluate(env).value()
        if left_value is None or right_value is None:
            raise TypeError("Cannot add non-numeric values")
        return Number(left_value + right_value)

    def value(self) -> Value | None:
        return None

    def __str__(self) -> str:
        return f"{self.left} + {self.right}"

    def __repr__(self) -> str:
        return f"Add({self.left!r}, {self.right!r})"


class Multiply(Expression):
    def __init__(self, left: Expression, right: Expression):
        self.left = left
        self.right = right

    def reduce(self, env: Environment) -> Expression:
        match (self.left.value(), self.right.value()):
            case (None, _):
                return Multiply(self.left.reduce(env), self.right)
            case (_, None):
                return Multiply(self.left, self.right.reduce(env))
            case (left, right):
                return Number(left * right)  # type: ignore

    def evaluate(self, env: Environment) -> Expression:
        left_value = self.left.evaluate(env).value()
        right_value = self.right.evaluate(env).value()
        if left_value is None or right_value is None:
            raise TypeError("Cannot add non-numeric values")
        return Number(left_value * right_value)

    def __str__(self) -> str:
        return f"{self.left} * {self.right}"

    def __repr__(self) -> str:
        return f"Multiply({self.left!r}, {self.right!r})"


class LessThan(Expression):
    def __init__(self, left: Expression, right: Expression):
        self.left = left
        self.right = right

    def reduce(self, env: Environment) -> Expression:
        match (self.left.value(), self.right.value()):
            case (None, _):
                return LessThan(self.left.reduce(env), self.right)
            case (_, None):
                return LessThan(self.left, self.right.reduce(env))
            case (left, right):
                return Boolean(left < right)  # type: ignore

    def evaluate(self, env: Environment) -> Expression:
        left_value = self.left.evaluate(env).value()
        right_value = self.right.evaluate(env).value()
        if left_value is None or right_value is None:
            raise TypeError("Cannot add non-numeric values")
        return Boolean(left_value < right_value)

    def __str__(self) -> str:
        return f"{self.left} < {self.right}"

    def __repr__(self) -> str:
        return f"LessThan({self.left!r}, {self.right!r})"


class Variable(Expression):
    def __init__(self, name: str):
        self.name = name

    def reduce(self, env: Environment) -> Expression:
        return env[self.name]

    def evaluate(self, env: Environment) -> Expression:
        return env[self.name]

    def __str__(self) -> str:
        return f"{self.name}"

    def __repr__(self) -> str:
        return f"Vairable({self.name})"


class DoNothing(Statement):
    def __init__(self) -> None:
        pass

    def reduce(self, env: Environment) -> tuple[Statement, Environment]:
        return (self, env)

    def evaluate(self, env: Environment) -> Environment:
        return env

    def __str__(self) -> str:
        return "do-nothing"

    def __repr__(self) -> str:
        return "DoNothing()"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, DoNothing)


class Assign(Statement):
    def __init__(self, name: str, exp: Expression):
        self.name = name
        self._exp = exp

    def reduce(self, env: Environment) -> tuple[Statement, Environment]:
        if self._exp.value() is None:
            return (Assign(self.name, self._exp.reduce(env)), env)
        else:
            return (DoNothing(), {**env, self.name: self._exp})

    def evaluate(self, env: Environment) -> Environment:
        return {**env, self.name: self._exp.evaluate(env)}

    def __str__(self) -> str:
        return f"{self.name} = {self._exp}"

    def __repr__(self) -> str:
        return f"Assign({self.name!r}, {self._exp!r})"


class If(Statement):
    def __init__(
        self, condition: Expression, consequence: Statement, alternative: Statement
    ) -> None:
        self._condition = condition
        self._consequence = consequence
        self._alternative = alternative

    def reduce(self, env: Environment) -> tuple[Statement, Environment]:
        match self._condition.value():
            case None:
                return (
                    If(
                        self._condition.reduce(env),
                        self._consequence,
                        self._alternative,
                    ),
                    env,
                )
            case True:
                return (self._consequence, env)
            case False:
                return (self._alternative, env)
            case _:
                raise ValueError(f"Invalid condition: {self._condition}")

    def evaluate(self, env: Environment) -> Environment:
        match self._condition.evaluate(env):
            case Boolean(v):
                if v:
                    return self._consequence.evaluate(env)
                else:
                    return self._alternative.evaluate(env)
            case _:
                raise ValueError(f"Invalid condition: {self._condition}")

    def __str__(self) -> str:
        return f"if {self._condition} {self._consequence} else {self._alternative}"

    def __repr__(self) -> str:
        return f"If({self._condition!r}, {self._consequence!r}, {self._alternative!r})"


class Sequence(Statement):
    def __init__(self, first: Statement, second: Statement) -> None:
        self._first = first
        self._second = second

    def reduce(self, env: Environment) -> tuple[Statement, Environment]:
        if isinstance(self._first, DoNothing):
            return (self._second, env)
        else:
            mod_first, mod_env = self._first.reduce(env)
            return (Sequence(mod_first, self._second), mod_env)

    def evaluate(self, env: Environment) -> Environment:
        return self._second.evaluate(self._first.evaluate(env))

    def __str__(self) -> str:
        return f"{self._first}; {self._second}"

    def __repr__(self) -> str:
        return f"Sequence({self._first!r}, {self._second!r})"


class While(Statement):
    def __init__(self, condition: Expression, body: Statement) -> None:
        self._condition = condition
        self._body = body

    def reduce(self, env: Environment) -> tuple[Statement, Environment]:
        return (If(self._condition, Sequence(self._body, self), DoNothing()), env)

    def evaluate(self, env: Environment) -> Environment:
        match self._condition.evaluate(env):
            case Boolean(True):
                return self.evaluate(self._body.evaluate(env))
            case Boolean(False):
                return env
            case _:
                raise ValueError(f"Invalid condition: {self._condition}")

    def __str__(self) -> str:
        return f"while {self._condition} {self._body}"

    def __repr__(self) -> str:
        return f"While({self._condition!r}, {self._body!r})"


class Eval:
    def __init__(self, exp: Expression, env: Environment) -> None:
        self._exp = exp
        self._env = env

    def _step(self) -> None:
        self._exp = self._exp.reduce(self._env)

    def _value(self) -> Value | None:
        return self._exp.value()

    def run(self) -> Value:
        org_exp = self._exp
        while self._value() is None:
            self._step()
        ret = self._value()
        if ret is None:
            raise ValueError(f"Wrong expression {org_exp!r}")
        return ret


class Machine:
    def __init__(self, statement: Statement, env: Environment) -> None:
        self._statement = statement
        self._env = env

    def _step(self) -> None:
        self._statement, self._env = self._statement.reduce(self._env)

    def run(self) -> None:
        while self._statement != DoNothing():
            self._step()

    @property
    def env(self) -> Environment:
        return {**self._env}


def test_reduce():
    exp = Add(Multiply(Number(1), Number(2)), Multiply(Number(3), Number(4)))

    env: Environment = {}
    assert exp.value() is None
    assert str(exp) == "1 * 2 + 3 * 4"
    exp = exp.reduce(env)
    assert str(exp) == "2 + 3 * 4"
    exp = exp.reduce(env)
    assert str(exp) == "2 + 12"
    exp = exp.reduce(env)
    assert str(exp) == "14"
    assert exp.value() == 14


@pytest.mark.parametrize(
    "exp, env, expected",
    [
        (Add(Multiply(Number(1), Number(2)), Multiply(Number(3), Number(4))), {}, 14),
        (LessThan(Number(5), Add(Number(2), Number(2))), {}, False),
        (LessThan(Number(5), Add(Number(2), Number(5))), {}, True),
        (Add(Variable("x"), Variable("y")), {"x": Number(3), "y": Number(4)}, 7),
        (
            While(
                LessThan(Variable("x"), Number(5)),
                Assign("x", Multiply(Variable("x"), Number(3))),
            ),
            {"x": Number(1)},
            {"x": Number(9)},
        ),
    ],
)
def test_expression(exp: Expression, env: Environment, expected: Environment):
    assert Eval(exp, env).run() == expected
    assert exp.evaluate(env) == expected


@pytest.mark.parametrize(
    "stmt, env, expected",
    [
        (
            Assign("x", Add(Variable("x"), Number(1))),
            {"x": Number(2)},
            {"x": Number(3)},
        ),
        (
            If(Variable("x"), Assign("y", Number(1)), Assign("y", Number(2))),
            {"x": Boolean(True)},
            {"x": Boolean(True), "y": Number(1)},
        ),
        (
            If(Variable("x"), Assign("y", Number(1)), Assign("y", Number(2))),
            {"x": Boolean(False)},
            {"x": Boolean(False), "y": Number(2)},
        ),
        (
            Sequence(
                Assign("x", Add(Number(1), Number(1))),
                Assign("y", Add(Variable("x"), Number(3))),
            ),
            {},
            {"x": Number(2), "y": Number(5)},
        ),
    ],
)
def test_statement(stmt: Statement, env: Environment, expected: Environment):
    machine = Machine(stmt, env)
    machine.run()
    assert machine.env == expected
    assert stmt.evaluate(env) == expected


class SimpleSemantics:
    def number(self, ast):
        return Number(int(ast))

    def bool(self, ast):
        bool_value = ast.lower() == "true"
        return Boolean(bool_value)

    def variable(self, ast):
        return Variable(str(ast))

    def less_than(self, ast):
        return LessThan(ast.left, ast.right)

    def add(self, ast):
        return Add(ast.left, ast.right)

    def multiply(self, ast):
        return Multiply(ast.left, ast.right)

    def assign(self, ast):
        return Assign(ast.name.name, ast.value)

    def while_(self, ast):
        return While(ast.condition, ast.body)

    def if_(self, ast):
        return If(ast.condition, ast.consequence, ast.alternative)

    def sequence(self, ast):
        return reduce(lambda rem, stmt: Sequence(stmt, rem), reversed(ast))


def test_parser():
    code = r"""
    i = 0
    x = 0
    while(i < 11) {
        x = x + i
        i = i + 1
    }
    """
    ast = tatsu.parse(GRAMMER, code, semantics=SimpleSemantics())
    assert ast.evaluate({"i": Number(0), "x": Number(0)}) == {"i": Number(11), "x": Number(55)}