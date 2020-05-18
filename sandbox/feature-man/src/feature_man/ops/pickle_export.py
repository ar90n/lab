import pickle
from pathlib import Path
from typing import List, Union

import numpy as np
from smart_open import open

from ..context import Context
from ..types import Shape
from .base import BaseOp


class PickleExport(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (None, None, None)

    @property
    def output_shape(self) -> Shape:
        return ()

    def __init__(self, path: Union[str, Path]) -> None:
        self.path = Path(path)

    def _compute_feature(
        self, context: Context, arrays: List[np.ndarray]
    ) -> List[np.ndarray]:
        with open(self.path, "wb") as f:
            pickle.dump(arrays, f)
        return []
