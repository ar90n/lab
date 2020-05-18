from abc import ABC, abstractmethod, abstractproperty
from typing import Optional, Literal, Tuple, List, overload, get_origin


Dim = Optional[int]
Shape = Tuple[Dim, ...]
Array = List



def validate_shape(expect: Shape, actual: Shape) -> None:
    if len(expect) != len(actual):
        raise ValueError('')
    is_valid = all((e == a for e,a in zip(expect, actual) if e is not None))
    if not is_valid:
        raise ValueError('')

class BaseOp(ABC):
    @overload
    def __call__(self, arg: Shape) -> Shape:
        ...

    @overload
    def __call__(self, arg: List[Array]) -> List[Array]:
        ...

    def __call__(self, arg):
        if type(arg) == get_origin(Shape):
            validate_shape(self.input_shape, arg)
            output_shape = self.compute_shape(arg)
            if self.output_shape is not None:
                validate_shape(self.output_shape, output_shape)
            return output_shape
        if type(arg) == get_origin(List[Array]):
            return self.compute_feature(arg)
        raise NotImplementedError("error")

    @abstractmethod
    def compute_shape(self, arg: Shape) -> Shape:
        ...

    @abstractmethod
    def compute_feature(self, arg: List[Array]) -> List[Array]:
        ...

    @abstractproperty
    def input_shape(self) -> Shape:
        ...

    @property
    def output_shape(self) -> Optional[Shape]:
        return None



class Derived1(Op):
    def compute_shape(self, arg: Shape) -> Shape:
        return (1, 2, 3)

    def compute_feature(self, arg: List[Array]) ->  List[Array]:
        return []

    @property
    def input_shape(self) -> Shape:
        return (None, 2, None)

    @property
    def output_shape(self) -> Shape:
        return (None, 3, 3)


class Derived2(Op):
    def compute_shape(self, arg: Shape) -> Shape:
        return (4, 2, 3)

    def compute_feature(self, arg: List[Array]) ->  List[Array]:
        return []

    @property
    def input_shape(self) -> Shape:
        return (1, 0, 0)


d1 = Derived1()
d2 = Derived2()
print(d1([]), d1((1, 2, 3)))
#print(d2([]), d2((1,)))
