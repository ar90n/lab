from typing import List, Union

import numpy as np

from ..context import Context
from ..types import Shape
from .base import BaseOp


class Concat(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (None, None, None)

    def __init__(self, axis: int) -> None:
        self.axis = axis

    def _compute_feature(
        self, context: Context, images: List[np.ndarray]
    ) -> List[np.ndarray]:
        return [np.concatenate(images, axis=self.axis)]
