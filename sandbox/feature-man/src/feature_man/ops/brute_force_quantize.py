import pickle
from enum import IntEnum
from pathlib import Path
from typing import List, Optional, Union

import cv2
import numpy as np
from smart_open import open

from ..types import Shape
from .base import BaseOp, Context


class BruteForceQuanize(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (None, None)

    @property
    def output_shape(self) -> Optional[Shape]:
        return (None, 1)

    class NormType(IntEnum):
        INF = cv2.NORM_INF
        L1 = cv2.NORM_L1
        L2 = cv2.NORM_L2
        L2SQR = cv2.NORM_L2SQR
        HAMMING = cv2.NORM_HAMMING
        HAMMING2 = cv2.NORM_HAMMING2

    def __init__(self, codebook: Union[str, Path], norm_type: NormType = NormType.L2):
        self.codebook = codebook
        self.norm_type = norm_type

    def _compute_feature(
        self, context: Context, arrays: List[np.ndarray]
    ) -> List[np.ndarray]:
        return [self._compute_codes(a) for a in arrays]

    def _compute_codes(self, array: np.ndarray) -> np.ndarray:
        with open(self.codebook, "rb") as f:
            codebook = pickle.load(f)
        bf = cv2.BFMatcher(self.norm_type, crossCheck=False)
        matches = bf.match(array, codebook[0])
        matches = sorted(matches, key=lambda x: x.queryIdx)
        return np.array([m.trainIdx for m in matches]).reshape(-1, 1)
