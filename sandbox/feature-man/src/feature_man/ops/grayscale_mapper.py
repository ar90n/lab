from typing import List, Optional, Union

import cv2
import numpy as np

from ..context import Context
from ..types import Shape
from .base import BaseOp


class GrayscaleMapper(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (3, None, None)

    @property
    def output_shape(self) -> Optional[Shape]:
        return (1, None, None)

    def __init__(self) -> None:
        pass

    def _compute_feature(
        self, context: Context, images: List[np.ndarray]
    ) -> List[np.ndarray]:
        return [self._convert_color(i) for i in images]

    def _convert_color(self, image: np.ndarray) -> np.ndarray:
        return cv2.cvtColor(image.transpose([1, 2, 0]), cv2.COLOR_BGR2GRAY,)[
            np.newaxis, :, :
        ]
