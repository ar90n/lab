from typing import Any
from returns.result import Failure, Success, Result
from returns.maybe import Maybe, Some, Nothing
from returns.io import IO

def double(state: int) -> int:
    return state * 2

result: Result[int, Any] = Success(1).map(double)
assert str(result) == '<Success: 2>'

result: Result[int, Any] = result.map(lambda state: state + 1)
assert str(result) == '<Success: 3>'

io = IO('bytes').map(list)
print(str(io))



one = IO(1)
two = IO(2)

from returns.curry import curry

@curry
def sum_two_numbers(first: int, second: int) -> int:
    return first + second

assert two.apply(one.apply(IO(sum_two_numbers))) == IO(3)

assert Failure(1).alt(str) == Failure('1')

def tolerate_exception(state: Exception) -> Result[int, Exception]:
    if isinstance(state, ZeroDivisionError):
        return Success(0)
    return Failure(state)

value: Result[int, Exception] = Failure(ZeroDivisionError())
result: Result[int ,Exception] = value.lash(tolerate_exception)
assert result == Success(0)

value2: Result[int, Exception] = Failure(ValueError())
result2: Result[int,  Exception]  = value2.lash(tolerate_exception)

assert Success(1).value_or(None) == 1
assert Some(0).unwrap() == 0

def from_a_to_b(arg: int) -> str:
    return str(arg)

assert from_a_to_b(1) == '1'

from typing import List
def all_to_str(arg: List[int]) -> List[str]:
    return [str(item) for item in arg]

assert all_to_str([1, 2]) == ['1', '2']


from typing import TypeVar
ValueType = TypeVar('ValueType')

def copy(arg: ValueType) -> ValueType:
    ...

from returns.interfaces.container import Container1
#def to_str(container: Container1[int]) -> Container1[str]:
#    return container.map(str)
#assert to_str(Maybe.from_value(1)) == Maybe.from_value('1')
#assert to_str(IO.from_value(1)) == IO.from_value('1')


from typing import overload

@overload
def to_str(arg: Maybe[int]) -> Maybe[str]:
    ...

@overload
def to_str(arg: IO[int]) -> IO[str]:
    ...
