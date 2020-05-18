import pickle
from pathlib import Path
from typing import List, Optional, Union

import cv2
import numpy as np

from ..context import Context
from ..types import Shape
from .base import BaseOp


class Histogram(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (None, 1)

    @property
    def output_shape(self) -> Optional[Shape]:
        return (1, self.n_bins)

    def __init__(self, n_bins: int) -> None:
        self.n_bins = n_bins

    def _compute_feature(
        self, context: Context, arrays: List[np.ndarray]
    ) -> List[np.ndarray]:
        return [self._compute_histogram(a) for a in arrays]

    def _compute_histogram(self, array: np.ndarray) -> np.ndarray:
        hist, _ = np.histogram(array, bins=range(self.n_bins + 1))
        return hist
