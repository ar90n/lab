from typing import List, Type, Union

import numpy as np

from ..context import Context
from ..types import Shape
from .base import BaseOp


class TeeContainer(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return self._op.input_shapes

    def __init__(self, op: Type[BaseOp]):
        if isinstance(input_shapes := op.input_shapes, list) and len(input_shapes) != 1:
            raise TypeError(f"mismatch length of input shape. {len(input_shapes)} != 1")
        self._op = op

    def _compute_feature(
        self, context: Context, arrays: List[np.ndarray]
    ) -> List[np.ndarray]:
        self._op(context, arrays)
        return arrays
